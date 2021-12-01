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
-- Module      : Amazonka.APIGateway.Types.MethodSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.MethodSnapshot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a summary of a Method resource, given a particular date and
-- time.
--
-- /See:/ 'newMethodSnapshot' smart constructor.
data MethodSnapshot = MethodSnapshot'
  { -- | The method\'s authorization type. Valid values are @NONE@ for open
    -- access, @AWS_IAM@ for using AWS IAM permissions, @CUSTOM@ for using a
    -- custom authorizer, or @COGNITO_USER_POOLS@ for using a Cognito user
    -- pool.
    authorizationType :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the method requires a valid ApiKey.
    apiKeyRequired :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MethodSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizationType', 'methodSnapshot_authorizationType' - The method\'s authorization type. Valid values are @NONE@ for open
-- access, @AWS_IAM@ for using AWS IAM permissions, @CUSTOM@ for using a
-- custom authorizer, or @COGNITO_USER_POOLS@ for using a Cognito user
-- pool.
--
-- 'apiKeyRequired', 'methodSnapshot_apiKeyRequired' - Specifies whether the method requires a valid ApiKey.
newMethodSnapshot ::
  MethodSnapshot
newMethodSnapshot =
  MethodSnapshot'
    { authorizationType =
        Prelude.Nothing,
      apiKeyRequired = Prelude.Nothing
    }

-- | The method\'s authorization type. Valid values are @NONE@ for open
-- access, @AWS_IAM@ for using AWS IAM permissions, @CUSTOM@ for using a
-- custom authorizer, or @COGNITO_USER_POOLS@ for using a Cognito user
-- pool.
methodSnapshot_authorizationType :: Lens.Lens' MethodSnapshot (Prelude.Maybe Prelude.Text)
methodSnapshot_authorizationType = Lens.lens (\MethodSnapshot' {authorizationType} -> authorizationType) (\s@MethodSnapshot' {} a -> s {authorizationType = a} :: MethodSnapshot)

-- | Specifies whether the method requires a valid ApiKey.
methodSnapshot_apiKeyRequired :: Lens.Lens' MethodSnapshot (Prelude.Maybe Prelude.Bool)
methodSnapshot_apiKeyRequired = Lens.lens (\MethodSnapshot' {apiKeyRequired} -> apiKeyRequired) (\s@MethodSnapshot' {} a -> s {apiKeyRequired = a} :: MethodSnapshot)

instance Core.FromJSON MethodSnapshot where
  parseJSON =
    Core.withObject
      "MethodSnapshot"
      ( \x ->
          MethodSnapshot'
            Prelude.<$> (x Core..:? "authorizationType")
            Prelude.<*> (x Core..:? "apiKeyRequired")
      )

instance Prelude.Hashable MethodSnapshot where
  hashWithSalt salt' MethodSnapshot' {..} =
    salt' `Prelude.hashWithSalt` apiKeyRequired
      `Prelude.hashWithSalt` authorizationType

instance Prelude.NFData MethodSnapshot where
  rnf MethodSnapshot' {..} =
    Prelude.rnf authorizationType
      `Prelude.seq` Prelude.rnf apiKeyRequired
