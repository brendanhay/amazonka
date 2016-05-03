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
-- Module      : Network.AWS.Inspector.DescribeCrossAccountAccessRole
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the IAM role that enables Inspector to access your AWS
-- account.
module Network.AWS.Inspector.DescribeCrossAccountAccessRole
    (
    -- * Creating a Request
      describeCrossAccountAccessRole
    , DescribeCrossAccountAccessRole

    -- * Destructuring the Response
    , describeCrossAccountAccessRoleResponse
    , DescribeCrossAccountAccessRoleResponse
    -- * Response Lenses
    , dcaarrsValid
    , dcaarrsRoleARN
    , dcaarrsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeCrossAccountAccessRole' smart constructor.
data DescribeCrossAccountAccessRole =
    DescribeCrossAccountAccessRole'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeCrossAccountAccessRole' with the minimum fields required to make a request.
--
describeCrossAccountAccessRole
    :: DescribeCrossAccountAccessRole
describeCrossAccountAccessRole = DescribeCrossAccountAccessRole'

instance AWSRequest DescribeCrossAccountAccessRole
         where
        type Rs DescribeCrossAccountAccessRole =
             DescribeCrossAccountAccessRoleResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 DescribeCrossAccountAccessRoleResponse' <$>
                   (x .?> "valid") <*> (x .?> "roleArn") <*>
                     (pure (fromEnum s)))

instance Hashable DescribeCrossAccountAccessRole

instance NFData DescribeCrossAccountAccessRole

instance ToHeaders DescribeCrossAccountAccessRole
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.DescribeCrossAccountAccessRole" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeCrossAccountAccessRole where
        toJSON = const (Object mempty)

instance ToPath DescribeCrossAccountAccessRole where
        toPath = const "/"

instance ToQuery DescribeCrossAccountAccessRole where
        toQuery = const mempty

-- | /See:/ 'describeCrossAccountAccessRoleResponse' smart constructor.
data DescribeCrossAccountAccessRoleResponse = DescribeCrossAccountAccessRoleResponse'
    { _dcaarrsValid          :: !(Maybe Bool)
    , _dcaarrsRoleARN        :: !(Maybe Text)
    , _dcaarrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeCrossAccountAccessRoleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcaarrsValid'
--
-- * 'dcaarrsRoleARN'
--
-- * 'dcaarrsResponseStatus'
describeCrossAccountAccessRoleResponse
    :: Int -- ^ 'dcaarrsResponseStatus'
    -> DescribeCrossAccountAccessRoleResponse
describeCrossAccountAccessRoleResponse pResponseStatus_ =
    DescribeCrossAccountAccessRoleResponse'
    { _dcaarrsValid = Nothing
    , _dcaarrsRoleARN = Nothing
    , _dcaarrsResponseStatus = pResponseStatus_
    }

-- | A Boolean value that specifies whether the IAM role has the necessary
-- policies attached to enable Inspector to access your AWS account.
dcaarrsValid :: Lens' DescribeCrossAccountAccessRoleResponse (Maybe Bool)
dcaarrsValid = lens _dcaarrsValid (\ s a -> s{_dcaarrsValid = a});

-- | The ARN specifying the IAM role that Inspector uses to access your AWS
-- account.
dcaarrsRoleARN :: Lens' DescribeCrossAccountAccessRoleResponse (Maybe Text)
dcaarrsRoleARN = lens _dcaarrsRoleARN (\ s a -> s{_dcaarrsRoleARN = a});

-- | The response status code.
dcaarrsResponseStatus :: Lens' DescribeCrossAccountAccessRoleResponse Int
dcaarrsResponseStatus = lens _dcaarrsResponseStatus (\ s a -> s{_dcaarrsResponseStatus = a});

instance NFData
         DescribeCrossAccountAccessRoleResponse
