{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.DescribeAssessmentTargets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the assessment targets that are specified by the ARNs of the assessment targets.
--
--
module Network.AWS.Inspector.DescribeAssessmentTargets
    (
    -- * Creating a Request
      describeAssessmentTargets
    , DescribeAssessmentTargets
    -- * Request Lenses
    , datAssessmentTargetARNs

    -- * Destructuring the Response
    , describeAssessmentTargetsResponse
    , DescribeAssessmentTargetsResponse
    -- * Response Lenses
    , drsResponseStatus
    , drsAssessmentTargets
    , drsFailedItems
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAssessmentTargets' smart constructor.
newtype DescribeAssessmentTargets = DescribeAssessmentTargets'
  { _datAssessmentTargetARNs :: List1 Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAssessmentTargets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'datAssessmentTargetARNs' - The ARNs that specifies the assessment targets that you want to describe.
describeAssessmentTargets
    :: NonEmpty Text -- ^ 'datAssessmentTargetARNs'
    -> DescribeAssessmentTargets
describeAssessmentTargets pAssessmentTargetARNs_ =
  DescribeAssessmentTargets'
    {_datAssessmentTargetARNs = _List1 # pAssessmentTargetARNs_}


-- | The ARNs that specifies the assessment targets that you want to describe.
datAssessmentTargetARNs :: Lens' DescribeAssessmentTargets (NonEmpty Text)
datAssessmentTargetARNs = lens _datAssessmentTargetARNs (\ s a -> s{_datAssessmentTargetARNs = a}) . _List1

instance AWSRequest DescribeAssessmentTargets where
        type Rs DescribeAssessmentTargets =
             DescribeAssessmentTargetsResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAssessmentTargetsResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .?> "assessmentTargets" .!@ mempty)
                     <*> (x .?> "failedItems" .!@ mempty))

instance Hashable DescribeAssessmentTargets where

instance NFData DescribeAssessmentTargets where

instance ToHeaders DescribeAssessmentTargets where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.DescribeAssessmentTargets" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeAssessmentTargets where
        toJSON DescribeAssessmentTargets'{..}
          = object
              (catMaybes
                 [Just
                    ("assessmentTargetArns" .=
                       _datAssessmentTargetARNs)])

instance ToPath DescribeAssessmentTargets where
        toPath = const "/"

instance ToQuery DescribeAssessmentTargets where
        toQuery = const mempty

-- | /See:/ 'describeAssessmentTargetsResponse' smart constructor.
data DescribeAssessmentTargetsResponse = DescribeAssessmentTargetsResponse'
  { _drsResponseStatus    :: !Int
  , _drsAssessmentTargets :: ![AssessmentTarget]
  , _drsFailedItems       :: !(Map Text FailedItemDetails)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAssessmentTargetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
--
-- * 'drsAssessmentTargets' - Information about the assessment targets.
--
-- * 'drsFailedItems' - Assessment target details that cannot be described. An error code is provided for each failed item.
describeAssessmentTargetsResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeAssessmentTargetsResponse
describeAssessmentTargetsResponse pResponseStatus_ =
  DescribeAssessmentTargetsResponse'
    { _drsResponseStatus = pResponseStatus_
    , _drsAssessmentTargets = mempty
    , _drsFailedItems = mempty
    }


-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeAssessmentTargetsResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

-- | Information about the assessment targets.
drsAssessmentTargets :: Lens' DescribeAssessmentTargetsResponse [AssessmentTarget]
drsAssessmentTargets = lens _drsAssessmentTargets (\ s a -> s{_drsAssessmentTargets = a}) . _Coerce

-- | Assessment target details that cannot be described. An error code is provided for each failed item.
drsFailedItems :: Lens' DescribeAssessmentTargetsResponse (HashMap Text FailedItemDetails)
drsFailedItems = lens _drsFailedItems (\ s a -> s{_drsFailedItems = a}) . _Map

instance NFData DescribeAssessmentTargetsResponse
         where
