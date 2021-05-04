{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ElasticSearch.Types.CognitoOptionsStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.CognitoOptionsStatus where

import Network.AWS.ElasticSearch.Types.CognitoOptions
import Network.AWS.ElasticSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON CognitoOptionsStatus where
  parseJSON =
    Prelude.withObject
      "CognitoOptionsStatus"
      ( \x ->
          CognitoOptionsStatus'
            Prelude.<$> (x Prelude..: "Options")
            Prelude.<*> (x Prelude..: "Status")
      )

instance Prelude.Hashable CognitoOptionsStatus

instance Prelude.NFData CognitoOptionsStatus
