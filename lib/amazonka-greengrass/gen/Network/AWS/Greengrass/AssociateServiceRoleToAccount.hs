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
-- Module      : Network.AWS.Greengrass.AssociateServiceRoleToAccount
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a role with your account. AWS Greengrass will use the role to access your Lambda functions and AWS IoT resources. This is necessary for deployments to succeed. The role must have at least minimum permissions in the policy ''AWSGreengrassResourceAccessRolePolicy''.
module Network.AWS.Greengrass.AssociateServiceRoleToAccount
    (
    -- * Creating a Request
      associateServiceRoleToAccount
    , AssociateServiceRoleToAccount
    -- * Request Lenses
    , asrtaRoleARN

    -- * Destructuring the Response
    , associateServiceRoleToAccountResponse
    , AssociateServiceRoleToAccountResponse
    -- * Response Lenses
    , asrtarsAssociatedAt
    , asrtarsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateServiceRoleToAccount' smart constructor.
newtype AssociateServiceRoleToAccount = AssociateServiceRoleToAccount'
  { _asrtaRoleARN :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateServiceRoleToAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asrtaRoleARN' - The ARN of the service role you wish to associate with your account.
associateServiceRoleToAccount
    :: AssociateServiceRoleToAccount
associateServiceRoleToAccount =
  AssociateServiceRoleToAccount' {_asrtaRoleARN = Nothing}


-- | The ARN of the service role you wish to associate with your account.
asrtaRoleARN :: Lens' AssociateServiceRoleToAccount (Maybe Text)
asrtaRoleARN = lens _asrtaRoleARN (\ s a -> s{_asrtaRoleARN = a})

instance AWSRequest AssociateServiceRoleToAccount
         where
        type Rs AssociateServiceRoleToAccount =
             AssociateServiceRoleToAccountResponse
        request = putJSON greengrass
        response
          = receiveJSON
              (\ s h x ->
                 AssociateServiceRoleToAccountResponse' <$>
                   (x .?> "AssociatedAt") <*> (pure (fromEnum s)))

instance Hashable AssociateServiceRoleToAccount where

instance NFData AssociateServiceRoleToAccount where

instance ToHeaders AssociateServiceRoleToAccount
         where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssociateServiceRoleToAccount where
        toJSON AssociateServiceRoleToAccount'{..}
          = object
              (catMaybes [("RoleArn" .=) <$> _asrtaRoleARN])

instance ToPath AssociateServiceRoleToAccount where
        toPath = const "/greengrass/servicerole"

instance ToQuery AssociateServiceRoleToAccount where
        toQuery = const mempty

-- | /See:/ 'associateServiceRoleToAccountResponse' smart constructor.
data AssociateServiceRoleToAccountResponse = AssociateServiceRoleToAccountResponse'
  { _asrtarsAssociatedAt   :: !(Maybe Text)
  , _asrtarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateServiceRoleToAccountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asrtarsAssociatedAt' - The time when the service role was associated with the account.
--
-- * 'asrtarsResponseStatus' - -- | The response status code.
associateServiceRoleToAccountResponse
    :: Int -- ^ 'asrtarsResponseStatus'
    -> AssociateServiceRoleToAccountResponse
associateServiceRoleToAccountResponse pResponseStatus_ =
  AssociateServiceRoleToAccountResponse'
    {_asrtarsAssociatedAt = Nothing, _asrtarsResponseStatus = pResponseStatus_}


-- | The time when the service role was associated with the account.
asrtarsAssociatedAt :: Lens' AssociateServiceRoleToAccountResponse (Maybe Text)
asrtarsAssociatedAt = lens _asrtarsAssociatedAt (\ s a -> s{_asrtarsAssociatedAt = a})

-- | -- | The response status code.
asrtarsResponseStatus :: Lens' AssociateServiceRoleToAccountResponse Int
asrtarsResponseStatus = lens _asrtarsResponseStatus (\ s a -> s{_asrtarsResponseStatus = a})

instance NFData AssociateServiceRoleToAccountResponse
         where
