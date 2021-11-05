{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AmplifyBackend.Types.LoginAuthConfigReqObj
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AmplifyBackend.Types.LoginAuthConfigReqObj where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The request object for this operation.
--
-- /See:/ 'newLoginAuthConfigReqObj' smart constructor.
data LoginAuthConfigReqObj = LoginAuthConfigReqObj'
  { -- | The web client ID for the Amazon Cognito user pools.
    awsUserPoolsWebClientId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Cognito identity pool ID used for the Amplify Admin UI login
    -- authorization.
    awsCognitoIdentityPoolId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Cognito user pool ID used for Amplify Admin UI login
    -- authentication.
    awsUserPoolsId :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region for the Amplify Admin UI login.
    awsCognitoRegion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoginAuthConfigReqObj' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsUserPoolsWebClientId', 'loginAuthConfigReqObj_awsUserPoolsWebClientId' - The web client ID for the Amazon Cognito user pools.
--
-- 'awsCognitoIdentityPoolId', 'loginAuthConfigReqObj_awsCognitoIdentityPoolId' - The Amazon Cognito identity pool ID used for the Amplify Admin UI login
-- authorization.
--
-- 'awsUserPoolsId', 'loginAuthConfigReqObj_awsUserPoolsId' - The Amazon Cognito user pool ID used for Amplify Admin UI login
-- authentication.
--
-- 'awsCognitoRegion', 'loginAuthConfigReqObj_awsCognitoRegion' - The AWS Region for the Amplify Admin UI login.
newLoginAuthConfigReqObj ::
  LoginAuthConfigReqObj
newLoginAuthConfigReqObj =
  LoginAuthConfigReqObj'
    { awsUserPoolsWebClientId =
        Prelude.Nothing,
      awsCognitoIdentityPoolId = Prelude.Nothing,
      awsUserPoolsId = Prelude.Nothing,
      awsCognitoRegion = Prelude.Nothing
    }

-- | The web client ID for the Amazon Cognito user pools.
loginAuthConfigReqObj_awsUserPoolsWebClientId :: Lens.Lens' LoginAuthConfigReqObj (Prelude.Maybe Prelude.Text)
loginAuthConfigReqObj_awsUserPoolsWebClientId = Lens.lens (\LoginAuthConfigReqObj' {awsUserPoolsWebClientId} -> awsUserPoolsWebClientId) (\s@LoginAuthConfigReqObj' {} a -> s {awsUserPoolsWebClientId = a} :: LoginAuthConfigReqObj)

-- | The Amazon Cognito identity pool ID used for the Amplify Admin UI login
-- authorization.
loginAuthConfigReqObj_awsCognitoIdentityPoolId :: Lens.Lens' LoginAuthConfigReqObj (Prelude.Maybe Prelude.Text)
loginAuthConfigReqObj_awsCognitoIdentityPoolId = Lens.lens (\LoginAuthConfigReqObj' {awsCognitoIdentityPoolId} -> awsCognitoIdentityPoolId) (\s@LoginAuthConfigReqObj' {} a -> s {awsCognitoIdentityPoolId = a} :: LoginAuthConfigReqObj)

-- | The Amazon Cognito user pool ID used for Amplify Admin UI login
-- authentication.
loginAuthConfigReqObj_awsUserPoolsId :: Lens.Lens' LoginAuthConfigReqObj (Prelude.Maybe Prelude.Text)
loginAuthConfigReqObj_awsUserPoolsId = Lens.lens (\LoginAuthConfigReqObj' {awsUserPoolsId} -> awsUserPoolsId) (\s@LoginAuthConfigReqObj' {} a -> s {awsUserPoolsId = a} :: LoginAuthConfigReqObj)

-- | The AWS Region for the Amplify Admin UI login.
loginAuthConfigReqObj_awsCognitoRegion :: Lens.Lens' LoginAuthConfigReqObj (Prelude.Maybe Prelude.Text)
loginAuthConfigReqObj_awsCognitoRegion = Lens.lens (\LoginAuthConfigReqObj' {awsCognitoRegion} -> awsCognitoRegion) (\s@LoginAuthConfigReqObj' {} a -> s {awsCognitoRegion = a} :: LoginAuthConfigReqObj)

instance Core.FromJSON LoginAuthConfigReqObj where
  parseJSON =
    Core.withObject
      "LoginAuthConfigReqObj"
      ( \x ->
          LoginAuthConfigReqObj'
            Prelude.<$> (x Core..:? "aws_user_pools_web_client_id")
            Prelude.<*> (x Core..:? "aws_cognito_identity_pool_id")
            Prelude.<*> (x Core..:? "aws_user_pools_id")
            Prelude.<*> (x Core..:? "aws_cognito_region")
      )

instance Prelude.Hashable LoginAuthConfigReqObj

instance Prelude.NFData LoginAuthConfigReqObj

instance Core.ToJSON LoginAuthConfigReqObj where
  toJSON LoginAuthConfigReqObj' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("aws_user_pools_web_client_id" Core..=)
              Prelude.<$> awsUserPoolsWebClientId,
            ("aws_cognito_identity_pool_id" Core..=)
              Prelude.<$> awsCognitoIdentityPoolId,
            ("aws_user_pools_id" Core..=)
              Prelude.<$> awsUserPoolsId,
            ("aws_cognito_region" Core..=)
              Prelude.<$> awsCognitoRegion
          ]
      )
