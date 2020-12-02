{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateWorkforce
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to update your workforce. You can use this operation to require that workers use specific IP addresses to work on tasks and to update your OpenID Connect (OIDC) Identity Provider (IdP) workforce configuration.
--
--
-- Use @SourceIpConfig@ to restrict worker access to tasks to a specific range of IP addresses. You specify allowed IP addresses by creating a list of up to ten <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> . By default, a workforce isn't restricted to specific IP addresses. If you specify a range of IP addresses, workers who attempt to access tasks using any IP address outside the specified range are denied and get a @Not Found@ error message on the worker portal.
--
-- Use @OidcConfig@ to update the configuration of a workforce created using your own OIDC IdP.
--
-- /Important:/ You can only update your OIDC IdP configuration when there are no work teams associated with your workforce. You can delete work teams using the operation.
--
-- After restricting access to a range of IP addresses or updating your OIDC IdP configuration with this operation, you can view details about your update workforce using the operation.
--
-- /Important:/ This operation only applies to private workforces.
module Network.AWS.SageMaker.UpdateWorkforce
  ( -- * Creating a Request
    updateWorkforce,
    UpdateWorkforce,

    -- * Request Lenses
    uwSourceIPConfig,
    uwOidcConfig,
    uwWorkforceName,

    -- * Destructuring the Response
    updateWorkforceResponse,
    UpdateWorkforceResponse,

    -- * Response Lenses
    updrsResponseStatus,
    updrsWorkforce,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'updateWorkforce' smart constructor.
data UpdateWorkforce = UpdateWorkforce'
  { _uwSourceIPConfig ::
      !(Maybe SourceIPConfig),
    _uwOidcConfig :: !(Maybe OidcConfig),
    _uwWorkforceName :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateWorkforce' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uwSourceIPConfig' - A list of one to ten worker IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ) that can be used to access tasks assigned to this workforce. Maximum: Ten CIDR values
--
-- * 'uwOidcConfig' - Use this parameter to update your OIDC Identity Provider (IdP) configuration for a workforce made using your own IdP.
--
-- * 'uwWorkforceName' - The name of the private workforce that you want to update. You can find your workforce name by using the operation.
updateWorkforce ::
  -- | 'uwWorkforceName'
  Text ->
  UpdateWorkforce
updateWorkforce pWorkforceName_ =
  UpdateWorkforce'
    { _uwSourceIPConfig = Nothing,
      _uwOidcConfig = Nothing,
      _uwWorkforceName = pWorkforceName_
    }

-- | A list of one to ten worker IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ) that can be used to access tasks assigned to this workforce. Maximum: Ten CIDR values
uwSourceIPConfig :: Lens' UpdateWorkforce (Maybe SourceIPConfig)
uwSourceIPConfig = lens _uwSourceIPConfig (\s a -> s {_uwSourceIPConfig = a})

-- | Use this parameter to update your OIDC Identity Provider (IdP) configuration for a workforce made using your own IdP.
uwOidcConfig :: Lens' UpdateWorkforce (Maybe OidcConfig)
uwOidcConfig = lens _uwOidcConfig (\s a -> s {_uwOidcConfig = a})

-- | The name of the private workforce that you want to update. You can find your workforce name by using the operation.
uwWorkforceName :: Lens' UpdateWorkforce Text
uwWorkforceName = lens _uwWorkforceName (\s a -> s {_uwWorkforceName = a})

instance AWSRequest UpdateWorkforce where
  type Rs UpdateWorkforce = UpdateWorkforceResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          UpdateWorkforceResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "Workforce")
      )

instance Hashable UpdateWorkforce

instance NFData UpdateWorkforce

instance ToHeaders UpdateWorkforce where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.UpdateWorkforce" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateWorkforce where
  toJSON UpdateWorkforce' {..} =
    object
      ( catMaybes
          [ ("SourceIpConfig" .=) <$> _uwSourceIPConfig,
            ("OidcConfig" .=) <$> _uwOidcConfig,
            Just ("WorkforceName" .= _uwWorkforceName)
          ]
      )

instance ToPath UpdateWorkforce where
  toPath = const "/"

instance ToQuery UpdateWorkforce where
  toQuery = const mempty

-- | /See:/ 'updateWorkforceResponse' smart constructor.
data UpdateWorkforceResponse = UpdateWorkforceResponse'
  { _updrsResponseStatus ::
      !Int,
    _updrsWorkforce :: !Workforce
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateWorkforceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'updrsResponseStatus' - -- | The response status code.
--
-- * 'updrsWorkforce' - A single private workforce. You can create one private work force in each AWS Region. By default, any workforce-related API operation used in a specific region will apply to the workforce created in that region. To learn how to create a private workforce, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce> .
updateWorkforceResponse ::
  -- | 'updrsResponseStatus'
  Int ->
  -- | 'updrsWorkforce'
  Workforce ->
  UpdateWorkforceResponse
updateWorkforceResponse pResponseStatus_ pWorkforce_ =
  UpdateWorkforceResponse'
    { _updrsResponseStatus = pResponseStatus_,
      _updrsWorkforce = pWorkforce_
    }

-- | -- | The response status code.
updrsResponseStatus :: Lens' UpdateWorkforceResponse Int
updrsResponseStatus = lens _updrsResponseStatus (\s a -> s {_updrsResponseStatus = a})

-- | A single private workforce. You can create one private work force in each AWS Region. By default, any workforce-related API operation used in a specific region will apply to the workforce created in that region. To learn how to create a private workforce, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce> .
updrsWorkforce :: Lens' UpdateWorkforceResponse Workforce
updrsWorkforce = lens _updrsWorkforce (\s a -> s {_updrsWorkforce = a})

instance NFData UpdateWorkforceResponse
