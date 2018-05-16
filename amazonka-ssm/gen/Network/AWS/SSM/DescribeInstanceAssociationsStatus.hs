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
-- Module      : Network.AWS.SSM.DescribeInstanceAssociationsStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The status of the associations for the instance(s).
--
--
module Network.AWS.SSM.DescribeInstanceAssociationsStatus
    (
    -- * Creating a Request
      describeInstanceAssociationsStatus
    , DescribeInstanceAssociationsStatus
    -- * Request Lenses
    , diasNextToken
    , diasMaxResults
    , diasInstanceId

    -- * Destructuring the Response
    , describeInstanceAssociationsStatusResponse
    , DescribeInstanceAssociationsStatusResponse
    -- * Response Lenses
    , diasrsInstanceAssociationStatusInfos
    , diasrsNextToken
    , diasrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'describeInstanceAssociationsStatus' smart constructor.
data DescribeInstanceAssociationsStatus = DescribeInstanceAssociationsStatus'
  { _diasNextToken  :: !(Maybe Text)
  , _diasMaxResults :: !(Maybe Nat)
  , _diasInstanceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInstanceAssociationsStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diasNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'diasMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- * 'diasInstanceId' - The instance IDs for which you want association status information.
describeInstanceAssociationsStatus
    :: Text -- ^ 'diasInstanceId'
    -> DescribeInstanceAssociationsStatus
describeInstanceAssociationsStatus pInstanceId_ =
  DescribeInstanceAssociationsStatus'
    { _diasNextToken = Nothing
    , _diasMaxResults = Nothing
    , _diasInstanceId = pInstanceId_
    }


-- | The token for the next set of items to return. (You received this token from a previous call.)
diasNextToken :: Lens' DescribeInstanceAssociationsStatus (Maybe Text)
diasNextToken = lens _diasNextToken (\ s a -> s{_diasNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
diasMaxResults :: Lens' DescribeInstanceAssociationsStatus (Maybe Natural)
diasMaxResults = lens _diasMaxResults (\ s a -> s{_diasMaxResults = a}) . mapping _Nat

-- | The instance IDs for which you want association status information.
diasInstanceId :: Lens' DescribeInstanceAssociationsStatus Text
diasInstanceId = lens _diasInstanceId (\ s a -> s{_diasInstanceId = a})

instance AWSRequest
           DescribeInstanceAssociationsStatus
         where
        type Rs DescribeInstanceAssociationsStatus =
             DescribeInstanceAssociationsStatusResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribeInstanceAssociationsStatusResponse' <$>
                   (x .?> "InstanceAssociationStatusInfos" .!@ mempty)
                     <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeInstanceAssociationsStatus
         where

instance NFData DescribeInstanceAssociationsStatus
         where

instance ToHeaders DescribeInstanceAssociationsStatus
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribeInstanceAssociationsStatus" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeInstanceAssociationsStatus
         where
        toJSON DescribeInstanceAssociationsStatus'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _diasNextToken,
                  ("MaxResults" .=) <$> _diasMaxResults,
                  Just ("InstanceId" .= _diasInstanceId)])

instance ToPath DescribeInstanceAssociationsStatus
         where
        toPath = const "/"

instance ToQuery DescribeInstanceAssociationsStatus
         where
        toQuery = const mempty

-- | /See:/ 'describeInstanceAssociationsStatusResponse' smart constructor.
data DescribeInstanceAssociationsStatusResponse = DescribeInstanceAssociationsStatusResponse'
  { _diasrsInstanceAssociationStatusInfos :: !(Maybe [InstanceAssociationStatusInfo])
  , _diasrsNextToken :: !(Maybe Text)
  , _diasrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInstanceAssociationsStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diasrsInstanceAssociationStatusInfos' - Status information about the association.
--
-- * 'diasrsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'diasrsResponseStatus' - -- | The response status code.
describeInstanceAssociationsStatusResponse
    :: Int -- ^ 'diasrsResponseStatus'
    -> DescribeInstanceAssociationsStatusResponse
describeInstanceAssociationsStatusResponse pResponseStatus_ =
  DescribeInstanceAssociationsStatusResponse'
    { _diasrsInstanceAssociationStatusInfos = Nothing
    , _diasrsNextToken = Nothing
    , _diasrsResponseStatus = pResponseStatus_
    }


-- | Status information about the association.
diasrsInstanceAssociationStatusInfos :: Lens' DescribeInstanceAssociationsStatusResponse [InstanceAssociationStatusInfo]
diasrsInstanceAssociationStatusInfos = lens _diasrsInstanceAssociationStatusInfos (\ s a -> s{_diasrsInstanceAssociationStatusInfos = a}) . _Default . _Coerce

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
diasrsNextToken :: Lens' DescribeInstanceAssociationsStatusResponse (Maybe Text)
diasrsNextToken = lens _diasrsNextToken (\ s a -> s{_diasrsNextToken = a})

-- | -- | The response status code.
diasrsResponseStatus :: Lens' DescribeInstanceAssociationsStatusResponse Int
diasrsResponseStatus = lens _diasrsResponseStatus (\ s a -> s{_diasrsResponseStatus = a})

instance NFData
           DescribeInstanceAssociationsStatusResponse
         where
