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
-- Module      : Network.AWS.ElasticSearch.Types.LogPublishingOptionsStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.LogPublishingOptionsStatus where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types.LogPublishingOption
import Network.AWS.ElasticSearch.Types.LogType
import Network.AWS.ElasticSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens

-- | The configured log publishing options for the domain and their current
-- status.
--
-- /See:/ 'newLogPublishingOptionsStatus' smart constructor.
data LogPublishingOptionsStatus = LogPublishingOptionsStatus'
  { -- | The status of the log publishing options for the Elasticsearch domain.
    -- See @OptionStatus@ for the status information that\'s included.
    status :: Core.Maybe OptionStatus,
    -- | The log publishing options configured for the Elasticsearch domain.
    options :: Core.Maybe (Core.HashMap LogType LogPublishingOption)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LogPublishingOptionsStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'logPublishingOptionsStatus_status' - The status of the log publishing options for the Elasticsearch domain.
-- See @OptionStatus@ for the status information that\'s included.
--
-- 'options', 'logPublishingOptionsStatus_options' - The log publishing options configured for the Elasticsearch domain.
newLogPublishingOptionsStatus ::
  LogPublishingOptionsStatus
newLogPublishingOptionsStatus =
  LogPublishingOptionsStatus'
    { status = Core.Nothing,
      options = Core.Nothing
    }

-- | The status of the log publishing options for the Elasticsearch domain.
-- See @OptionStatus@ for the status information that\'s included.
logPublishingOptionsStatus_status :: Lens.Lens' LogPublishingOptionsStatus (Core.Maybe OptionStatus)
logPublishingOptionsStatus_status = Lens.lens (\LogPublishingOptionsStatus' {status} -> status) (\s@LogPublishingOptionsStatus' {} a -> s {status = a} :: LogPublishingOptionsStatus)

-- | The log publishing options configured for the Elasticsearch domain.
logPublishingOptionsStatus_options :: Lens.Lens' LogPublishingOptionsStatus (Core.Maybe (Core.HashMap LogType LogPublishingOption))
logPublishingOptionsStatus_options = Lens.lens (\LogPublishingOptionsStatus' {options} -> options) (\s@LogPublishingOptionsStatus' {} a -> s {options = a} :: LogPublishingOptionsStatus) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON LogPublishingOptionsStatus where
  parseJSON =
    Core.withObject
      "LogPublishingOptionsStatus"
      ( \x ->
          LogPublishingOptionsStatus'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "Options" Core..!= Core.mempty)
      )

instance Core.Hashable LogPublishingOptionsStatus

instance Core.NFData LogPublishingOptionsStatus
