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
-- Module      : Network.AWS.APIGateway.Types.MethodSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.MethodSnapshot where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a summary of a Method resource, given a particular date and
-- time.
--
-- /See:/ 'newMethodSnapshot' smart constructor.
data MethodSnapshot = MethodSnapshot'
  { -- | Specifies whether the method requires a valid ApiKey.
    apiKeyRequired :: Core.Maybe Core.Bool,
    -- | The method\'s authorization type. Valid values are @NONE@ for open
    -- access, @AWS_IAM@ for using AWS IAM permissions, @CUSTOM@ for using a
    -- custom authorizer, or @COGNITO_USER_POOLS@ for using a Cognito user
    -- pool.
    authorizationType :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MethodSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiKeyRequired', 'methodSnapshot_apiKeyRequired' - Specifies whether the method requires a valid ApiKey.
--
-- 'authorizationType', 'methodSnapshot_authorizationType' - The method\'s authorization type. Valid values are @NONE@ for open
-- access, @AWS_IAM@ for using AWS IAM permissions, @CUSTOM@ for using a
-- custom authorizer, or @COGNITO_USER_POOLS@ for using a Cognito user
-- pool.
newMethodSnapshot ::
  MethodSnapshot
newMethodSnapshot =
  MethodSnapshot'
    { apiKeyRequired = Core.Nothing,
      authorizationType = Core.Nothing
    }

-- | Specifies whether the method requires a valid ApiKey.
methodSnapshot_apiKeyRequired :: Lens.Lens' MethodSnapshot (Core.Maybe Core.Bool)
methodSnapshot_apiKeyRequired = Lens.lens (\MethodSnapshot' {apiKeyRequired} -> apiKeyRequired) (\s@MethodSnapshot' {} a -> s {apiKeyRequired = a} :: MethodSnapshot)

-- | The method\'s authorization type. Valid values are @NONE@ for open
-- access, @AWS_IAM@ for using AWS IAM permissions, @CUSTOM@ for using a
-- custom authorizer, or @COGNITO_USER_POOLS@ for using a Cognito user
-- pool.
methodSnapshot_authorizationType :: Lens.Lens' MethodSnapshot (Core.Maybe Core.Text)
methodSnapshot_authorizationType = Lens.lens (\MethodSnapshot' {authorizationType} -> authorizationType) (\s@MethodSnapshot' {} a -> s {authorizationType = a} :: MethodSnapshot)

instance Core.FromJSON MethodSnapshot where
  parseJSON =
    Core.withObject
      "MethodSnapshot"
      ( \x ->
          MethodSnapshot'
            Core.<$> (x Core..:? "apiKeyRequired")
            Core.<*> (x Core..:? "authorizationType")
      )

instance Core.Hashable MethodSnapshot

instance Core.NFData MethodSnapshot
