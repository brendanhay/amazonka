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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.MethodSnapshot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a summary of a Method resource, given a particular date and
-- time.
--
-- /See:/ 'newMethodSnapshot' smart constructor.
data MethodSnapshot = MethodSnapshot'
  { -- | Specifies whether the method requires a valid ApiKey.
    apiKeyRequired :: Prelude.Maybe Prelude.Bool,
    -- | The method\'s authorization type. Valid values are @NONE@ for open
    -- access, @AWS_IAM@ for using AWS IAM permissions, @CUSTOM@ for using a
    -- custom authorizer, or @COGNITO_USER_POOLS@ for using a Cognito user
    -- pool.
    authorizationType :: Prelude.Maybe Prelude.Text
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
    { apiKeyRequired = Prelude.Nothing,
      authorizationType = Prelude.Nothing
    }

-- | Specifies whether the method requires a valid ApiKey.
methodSnapshot_apiKeyRequired :: Lens.Lens' MethodSnapshot (Prelude.Maybe Prelude.Bool)
methodSnapshot_apiKeyRequired = Lens.lens (\MethodSnapshot' {apiKeyRequired} -> apiKeyRequired) (\s@MethodSnapshot' {} a -> s {apiKeyRequired = a} :: MethodSnapshot)

-- | The method\'s authorization type. Valid values are @NONE@ for open
-- access, @AWS_IAM@ for using AWS IAM permissions, @CUSTOM@ for using a
-- custom authorizer, or @COGNITO_USER_POOLS@ for using a Cognito user
-- pool.
methodSnapshot_authorizationType :: Lens.Lens' MethodSnapshot (Prelude.Maybe Prelude.Text)
methodSnapshot_authorizationType = Lens.lens (\MethodSnapshot' {authorizationType} -> authorizationType) (\s@MethodSnapshot' {} a -> s {authorizationType = a} :: MethodSnapshot)

instance Data.FromJSON MethodSnapshot where
  parseJSON =
    Data.withObject
      "MethodSnapshot"
      ( \x ->
          MethodSnapshot'
            Prelude.<$> (x Data..:? "apiKeyRequired")
            Prelude.<*> (x Data..:? "authorizationType")
      )

instance Prelude.Hashable MethodSnapshot where
  hashWithSalt _salt MethodSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` apiKeyRequired
      `Prelude.hashWithSalt` authorizationType

instance Prelude.NFData MethodSnapshot where
  rnf MethodSnapshot' {..} =
    Prelude.rnf apiKeyRequired
      `Prelude.seq` Prelude.rnf authorizationType
