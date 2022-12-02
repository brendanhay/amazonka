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
-- Module      : Amazonka.ElasticSearch.Types.CognitoOptionsStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.CognitoOptionsStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.CognitoOptions
import Amazonka.ElasticSearch.Types.OptionStatus
import qualified Amazonka.Prelude as Prelude

-- | Status of the Cognito options for the specified Elasticsearch domain.
--
-- /See:/ 'newCognitoOptionsStatus' smart constructor.
data CognitoOptionsStatus = CognitoOptionsStatus'
  { -- | Specifies the Cognito options for the specified Elasticsearch domain.
    options :: CognitoOptions,
    -- | Specifies the status of the Cognito options for the specified
    -- Elasticsearch domain.
    status :: OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CognitoOptionsStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'cognitoOptionsStatus_options' - Specifies the Cognito options for the specified Elasticsearch domain.
--
-- 'status', 'cognitoOptionsStatus_status' - Specifies the status of the Cognito options for the specified
-- Elasticsearch domain.
newCognitoOptionsStatus ::
  -- | 'options'
  CognitoOptions ->
  -- | 'status'
  OptionStatus ->
  CognitoOptionsStatus
newCognitoOptionsStatus pOptions_ pStatus_ =
  CognitoOptionsStatus'
    { options = pOptions_,
      status = pStatus_
    }

-- | Specifies the Cognito options for the specified Elasticsearch domain.
cognitoOptionsStatus_options :: Lens.Lens' CognitoOptionsStatus CognitoOptions
cognitoOptionsStatus_options = Lens.lens (\CognitoOptionsStatus' {options} -> options) (\s@CognitoOptionsStatus' {} a -> s {options = a} :: CognitoOptionsStatus)

-- | Specifies the status of the Cognito options for the specified
-- Elasticsearch domain.
cognitoOptionsStatus_status :: Lens.Lens' CognitoOptionsStatus OptionStatus
cognitoOptionsStatus_status = Lens.lens (\CognitoOptionsStatus' {status} -> status) (\s@CognitoOptionsStatus' {} a -> s {status = a} :: CognitoOptionsStatus)

instance Data.FromJSON CognitoOptionsStatus where
  parseJSON =
    Data.withObject
      "CognitoOptionsStatus"
      ( \x ->
          CognitoOptionsStatus'
            Prelude.<$> (x Data..: "Options")
            Prelude.<*> (x Data..: "Status")
      )

instance Prelude.Hashable CognitoOptionsStatus where
  hashWithSalt _salt CognitoOptionsStatus' {..} =
    _salt `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` status

instance Prelude.NFData CognitoOptionsStatus where
  rnf CognitoOptionsStatus' {..} =
    Prelude.rnf options
      `Prelude.seq` Prelude.rnf status
