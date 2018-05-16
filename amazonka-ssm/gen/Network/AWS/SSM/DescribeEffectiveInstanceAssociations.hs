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
-- Module      : Network.AWS.SSM.DescribeEffectiveInstanceAssociations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- All associations for the instance(s).
--
--
module Network.AWS.SSM.DescribeEffectiveInstanceAssociations
    (
    -- * Creating a Request
      describeEffectiveInstanceAssociations
    , DescribeEffectiveInstanceAssociations
    -- * Request Lenses
    , deiaNextToken
    , deiaMaxResults
    , deiaInstanceId

    -- * Destructuring the Response
    , describeEffectiveInstanceAssociationsResponse
    , DescribeEffectiveInstanceAssociationsResponse
    -- * Response Lenses
    , deiarsNextToken
    , deiarsAssociations
    , deiarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'describeEffectiveInstanceAssociations' smart constructor.
data DescribeEffectiveInstanceAssociations = DescribeEffectiveInstanceAssociations'
  { _deiaNextToken  :: !(Maybe Text)
  , _deiaMaxResults :: !(Maybe Nat)
  , _deiaInstanceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEffectiveInstanceAssociations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deiaNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'deiaMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- * 'deiaInstanceId' - The instance ID for which you want to view all associations.
describeEffectiveInstanceAssociations
    :: Text -- ^ 'deiaInstanceId'
    -> DescribeEffectiveInstanceAssociations
describeEffectiveInstanceAssociations pInstanceId_ =
  DescribeEffectiveInstanceAssociations'
    { _deiaNextToken = Nothing
    , _deiaMaxResults = Nothing
    , _deiaInstanceId = pInstanceId_
    }


-- | The token for the next set of items to return. (You received this token from a previous call.)
deiaNextToken :: Lens' DescribeEffectiveInstanceAssociations (Maybe Text)
deiaNextToken = lens _deiaNextToken (\ s a -> s{_deiaNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
deiaMaxResults :: Lens' DescribeEffectiveInstanceAssociations (Maybe Natural)
deiaMaxResults = lens _deiaMaxResults (\ s a -> s{_deiaMaxResults = a}) . mapping _Nat

-- | The instance ID for which you want to view all associations.
deiaInstanceId :: Lens' DescribeEffectiveInstanceAssociations Text
deiaInstanceId = lens _deiaInstanceId (\ s a -> s{_deiaInstanceId = a})

instance AWSRequest
           DescribeEffectiveInstanceAssociations
         where
        type Rs DescribeEffectiveInstanceAssociations =
             DescribeEffectiveInstanceAssociationsResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEffectiveInstanceAssociationsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Associations" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable
           DescribeEffectiveInstanceAssociations
         where

instance NFData DescribeEffectiveInstanceAssociations
         where

instance ToHeaders
           DescribeEffectiveInstanceAssociations
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribeEffectiveInstanceAssociations" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEffectiveInstanceAssociations
         where
        toJSON DescribeEffectiveInstanceAssociations'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _deiaNextToken,
                  ("MaxResults" .=) <$> _deiaMaxResults,
                  Just ("InstanceId" .= _deiaInstanceId)])

instance ToPath DescribeEffectiveInstanceAssociations
         where
        toPath = const "/"

instance ToQuery
           DescribeEffectiveInstanceAssociations
         where
        toQuery = const mempty

-- | /See:/ 'describeEffectiveInstanceAssociationsResponse' smart constructor.
data DescribeEffectiveInstanceAssociationsResponse = DescribeEffectiveInstanceAssociationsResponse'
  { _deiarsNextToken      :: !(Maybe Text)
  , _deiarsAssociations   :: !(Maybe [InstanceAssociation])
  , _deiarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEffectiveInstanceAssociationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deiarsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'deiarsAssociations' - The associations for the requested instance.
--
-- * 'deiarsResponseStatus' - -- | The response status code.
describeEffectiveInstanceAssociationsResponse
    :: Int -- ^ 'deiarsResponseStatus'
    -> DescribeEffectiveInstanceAssociationsResponse
describeEffectiveInstanceAssociationsResponse pResponseStatus_ =
  DescribeEffectiveInstanceAssociationsResponse'
    { _deiarsNextToken = Nothing
    , _deiarsAssociations = Nothing
    , _deiarsResponseStatus = pResponseStatus_
    }


-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
deiarsNextToken :: Lens' DescribeEffectiveInstanceAssociationsResponse (Maybe Text)
deiarsNextToken = lens _deiarsNextToken (\ s a -> s{_deiarsNextToken = a})

-- | The associations for the requested instance.
deiarsAssociations :: Lens' DescribeEffectiveInstanceAssociationsResponse [InstanceAssociation]
deiarsAssociations = lens _deiarsAssociations (\ s a -> s{_deiarsAssociations = a}) . _Default . _Coerce

-- | -- | The response status code.
deiarsResponseStatus :: Lens' DescribeEffectiveInstanceAssociationsResponse Int
deiarsResponseStatus = lens _deiarsResponseStatus (\ s a -> s{_deiarsResponseStatus = a})

instance NFData
           DescribeEffectiveInstanceAssociationsResponse
         where
