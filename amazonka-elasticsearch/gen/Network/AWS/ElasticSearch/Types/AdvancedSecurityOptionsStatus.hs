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
-- Module      : Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsStatus where

import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptions
import Network.AWS.ElasticSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the status of advanced security options for the specified
-- Elasticsearch domain.
--
-- /See:/ 'newAdvancedSecurityOptionsStatus' smart constructor.
data AdvancedSecurityOptionsStatus = AdvancedSecurityOptionsStatus'
  { -- | Specifies advanced security options for the specified Elasticsearch
    -- domain.
    options :: AdvancedSecurityOptions,
    -- | Status of the advanced security options for the specified Elasticsearch
    -- domain.
    status :: OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdvancedSecurityOptionsStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'advancedSecurityOptionsStatus_options' - Specifies advanced security options for the specified Elasticsearch
-- domain.
--
-- 'status', 'advancedSecurityOptionsStatus_status' - Status of the advanced security options for the specified Elasticsearch
-- domain.
newAdvancedSecurityOptionsStatus ::
  -- | 'options'
  AdvancedSecurityOptions ->
  -- | 'status'
  OptionStatus ->
  AdvancedSecurityOptionsStatus
newAdvancedSecurityOptionsStatus pOptions_ pStatus_ =
  AdvancedSecurityOptionsStatus'
    { options = pOptions_,
      status = pStatus_
    }

-- | Specifies advanced security options for the specified Elasticsearch
-- domain.
advancedSecurityOptionsStatus_options :: Lens.Lens' AdvancedSecurityOptionsStatus AdvancedSecurityOptions
advancedSecurityOptionsStatus_options = Lens.lens (\AdvancedSecurityOptionsStatus' {options} -> options) (\s@AdvancedSecurityOptionsStatus' {} a -> s {options = a} :: AdvancedSecurityOptionsStatus)

-- | Status of the advanced security options for the specified Elasticsearch
-- domain.
advancedSecurityOptionsStatus_status :: Lens.Lens' AdvancedSecurityOptionsStatus OptionStatus
advancedSecurityOptionsStatus_status = Lens.lens (\AdvancedSecurityOptionsStatus' {status} -> status) (\s@AdvancedSecurityOptionsStatus' {} a -> s {status = a} :: AdvancedSecurityOptionsStatus)

instance
  Prelude.FromJSON
    AdvancedSecurityOptionsStatus
  where
  parseJSON =
    Prelude.withObject
      "AdvancedSecurityOptionsStatus"
      ( \x ->
          AdvancedSecurityOptionsStatus'
            Prelude.<$> (x Prelude..: "Options")
            Prelude.<*> (x Prelude..: "Status")
      )

instance
  Prelude.Hashable
    AdvancedSecurityOptionsStatus

instance Prelude.NFData AdvancedSecurityOptionsStatus
