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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the IAM role that enables Amazon Inspector to access your AWS account.
--
--
module Network.AWS.Inspector.DescribeCrossAccountAccessRole
    (
    -- * Creating a Request
      describeCrossAccountAccessRole
    , DescribeCrossAccountAccessRole

    -- * Destructuring the Response
    , describeCrossAccountAccessRoleResponse
    , DescribeCrossAccountAccessRoleResponse
    -- * Response Lenses
    , dcaarrsResponseStatus
    , dcaarrsRoleARN
    , dcaarrsValid
    , dcaarrsRegisteredAt
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeCrossAccountAccessRole' smart constructor.
data DescribeCrossAccountAccessRole =
  DescribeCrossAccountAccessRole'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


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
                   (pure (fromEnum s)) <*> (x .:> "roleArn") <*>
                     (x .:> "valid")
                     <*> (x .:> "registeredAt"))

instance Hashable DescribeCrossAccountAccessRole
         where

instance NFData DescribeCrossAccountAccessRole where

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
  { _dcaarrsResponseStatus :: !Int
  , _dcaarrsRoleARN        :: !Text
  , _dcaarrsValid          :: !Bool
  , _dcaarrsRegisteredAt   :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCrossAccountAccessRoleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcaarrsResponseStatus' - -- | The response status code.
--
-- * 'dcaarrsRoleARN' - The ARN that specifies the IAM role that Amazon Inspector uses to access your AWS account.
--
-- * 'dcaarrsValid' - A Boolean value that specifies whether the IAM role has the necessary policies attached to enable Amazon Inspector to access your AWS account.
--
-- * 'dcaarrsRegisteredAt' - The date when the cross-account access role was registered.
describeCrossAccountAccessRoleResponse
    :: Int -- ^ 'dcaarrsResponseStatus'
    -> Text -- ^ 'dcaarrsRoleARN'
    -> Bool -- ^ 'dcaarrsValid'
    -> UTCTime -- ^ 'dcaarrsRegisteredAt'
    -> DescribeCrossAccountAccessRoleResponse
describeCrossAccountAccessRoleResponse pResponseStatus_ pRoleARN_ pValid_ pRegisteredAt_ =
  DescribeCrossAccountAccessRoleResponse'
    { _dcaarrsResponseStatus = pResponseStatus_
    , _dcaarrsRoleARN = pRoleARN_
    , _dcaarrsValid = pValid_
    , _dcaarrsRegisteredAt = _Time # pRegisteredAt_
    }


-- | -- | The response status code.
dcaarrsResponseStatus :: Lens' DescribeCrossAccountAccessRoleResponse Int
dcaarrsResponseStatus = lens _dcaarrsResponseStatus (\ s a -> s{_dcaarrsResponseStatus = a})

-- | The ARN that specifies the IAM role that Amazon Inspector uses to access your AWS account.
dcaarrsRoleARN :: Lens' DescribeCrossAccountAccessRoleResponse Text
dcaarrsRoleARN = lens _dcaarrsRoleARN (\ s a -> s{_dcaarrsRoleARN = a})

-- | A Boolean value that specifies whether the IAM role has the necessary policies attached to enable Amazon Inspector to access your AWS account.
dcaarrsValid :: Lens' DescribeCrossAccountAccessRoleResponse Bool
dcaarrsValid = lens _dcaarrsValid (\ s a -> s{_dcaarrsValid = a})

-- | The date when the cross-account access role was registered.
dcaarrsRegisteredAt :: Lens' DescribeCrossAccountAccessRoleResponse UTCTime
dcaarrsRegisteredAt = lens _dcaarrsRegisteredAt (\ s a -> s{_dcaarrsRegisteredAt = a}) . _Time

instance NFData
           DescribeCrossAccountAccessRoleResponse
         where
