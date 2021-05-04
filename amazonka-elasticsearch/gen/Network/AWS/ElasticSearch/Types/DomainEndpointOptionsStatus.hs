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
-- Module      : Network.AWS.ElasticSearch.Types.DomainEndpointOptionsStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.DomainEndpointOptionsStatus where

import Network.AWS.ElasticSearch.Types.DomainEndpointOptions
import Network.AWS.ElasticSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The configured endpoint options for the domain and their current status.
--
-- /See:/ 'newDomainEndpointOptionsStatus' smart constructor.
data DomainEndpointOptionsStatus = DomainEndpointOptionsStatus'
  { -- | Options to configure endpoint for the Elasticsearch domain.
    options :: DomainEndpointOptions,
    -- | The status of the endpoint options for the Elasticsearch domain. See
    -- @OptionStatus@ for the status information that\'s included.
    status :: OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DomainEndpointOptionsStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'domainEndpointOptionsStatus_options' - Options to configure endpoint for the Elasticsearch domain.
--
-- 'status', 'domainEndpointOptionsStatus_status' - The status of the endpoint options for the Elasticsearch domain. See
-- @OptionStatus@ for the status information that\'s included.
newDomainEndpointOptionsStatus ::
  -- | 'options'
  DomainEndpointOptions ->
  -- | 'status'
  OptionStatus ->
  DomainEndpointOptionsStatus
newDomainEndpointOptionsStatus pOptions_ pStatus_ =
  DomainEndpointOptionsStatus'
    { options = pOptions_,
      status = pStatus_
    }

-- | Options to configure endpoint for the Elasticsearch domain.
domainEndpointOptionsStatus_options :: Lens.Lens' DomainEndpointOptionsStatus DomainEndpointOptions
domainEndpointOptionsStatus_options = Lens.lens (\DomainEndpointOptionsStatus' {options} -> options) (\s@DomainEndpointOptionsStatus' {} a -> s {options = a} :: DomainEndpointOptionsStatus)

-- | The status of the endpoint options for the Elasticsearch domain. See
-- @OptionStatus@ for the status information that\'s included.
domainEndpointOptionsStatus_status :: Lens.Lens' DomainEndpointOptionsStatus OptionStatus
domainEndpointOptionsStatus_status = Lens.lens (\DomainEndpointOptionsStatus' {status} -> status) (\s@DomainEndpointOptionsStatus' {} a -> s {status = a} :: DomainEndpointOptionsStatus)

instance Prelude.FromJSON DomainEndpointOptionsStatus where
  parseJSON =
    Prelude.withObject
      "DomainEndpointOptionsStatus"
      ( \x ->
          DomainEndpointOptionsStatus'
            Prelude.<$> (x Prelude..: "Options")
            Prelude.<*> (x Prelude..: "Status")
      )

instance Prelude.Hashable DomainEndpointOptionsStatus

instance Prelude.NFData DomainEndpointOptionsStatus
