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
-- Module      : Amazonka.SES.Types.ReputationOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.ReputationOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the reputation settings for a configuration
-- set.
--
-- /See:/ 'newReputationOptions' smart constructor.
data ReputationOptions = ReputationOptions'
  { -- | The date and time at which the reputation metrics for the configuration
    -- set were last reset. Resetting these metrics is known as a /fresh
    -- start/.
    --
    -- When you disable email sending for a configuration set using
    -- UpdateConfigurationSetSendingEnabled and later re-enable it, the
    -- reputation metrics for the configuration set (but not for the entire
    -- Amazon SES account) are reset.
    --
    -- If email sending for the configuration set has never been disabled and
    -- later re-enabled, the value of this attribute is @null@.
    lastFreshStart :: Prelude.Maybe Data.ISO8601,
    -- | Describes whether or not Amazon SES publishes reputation metrics for the
    -- configuration set, such as bounce and complaint rates, to Amazon
    -- CloudWatch.
    --
    -- If the value is @true@, reputation metrics are published. If the value
    -- is @false@, reputation metrics are not published. The default value is
    -- @false@.
    reputationMetricsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Describes whether email sending is enabled or disabled for the
    -- configuration set. If the value is @true@, then Amazon SES will send
    -- emails that use the configuration set. If the value is @false@, Amazon
    -- SES will not send emails that use the configuration set. The default
    -- value is @true@. You can change this setting using
    -- UpdateConfigurationSetSendingEnabled.
    sendingEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReputationOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastFreshStart', 'reputationOptions_lastFreshStart' - The date and time at which the reputation metrics for the configuration
-- set were last reset. Resetting these metrics is known as a /fresh
-- start/.
--
-- When you disable email sending for a configuration set using
-- UpdateConfigurationSetSendingEnabled and later re-enable it, the
-- reputation metrics for the configuration set (but not for the entire
-- Amazon SES account) are reset.
--
-- If email sending for the configuration set has never been disabled and
-- later re-enabled, the value of this attribute is @null@.
--
-- 'reputationMetricsEnabled', 'reputationOptions_reputationMetricsEnabled' - Describes whether or not Amazon SES publishes reputation metrics for the
-- configuration set, such as bounce and complaint rates, to Amazon
-- CloudWatch.
--
-- If the value is @true@, reputation metrics are published. If the value
-- is @false@, reputation metrics are not published. The default value is
-- @false@.
--
-- 'sendingEnabled', 'reputationOptions_sendingEnabled' - Describes whether email sending is enabled or disabled for the
-- configuration set. If the value is @true@, then Amazon SES will send
-- emails that use the configuration set. If the value is @false@, Amazon
-- SES will not send emails that use the configuration set. The default
-- value is @true@. You can change this setting using
-- UpdateConfigurationSetSendingEnabled.
newReputationOptions ::
  ReputationOptions
newReputationOptions =
  ReputationOptions'
    { lastFreshStart =
        Prelude.Nothing,
      reputationMetricsEnabled = Prelude.Nothing,
      sendingEnabled = Prelude.Nothing
    }

-- | The date and time at which the reputation metrics for the configuration
-- set were last reset. Resetting these metrics is known as a /fresh
-- start/.
--
-- When you disable email sending for a configuration set using
-- UpdateConfigurationSetSendingEnabled and later re-enable it, the
-- reputation metrics for the configuration set (but not for the entire
-- Amazon SES account) are reset.
--
-- If email sending for the configuration set has never been disabled and
-- later re-enabled, the value of this attribute is @null@.
reputationOptions_lastFreshStart :: Lens.Lens' ReputationOptions (Prelude.Maybe Prelude.UTCTime)
reputationOptions_lastFreshStart = Lens.lens (\ReputationOptions' {lastFreshStart} -> lastFreshStart) (\s@ReputationOptions' {} a -> s {lastFreshStart = a} :: ReputationOptions) Prelude.. Lens.mapping Data._Time

-- | Describes whether or not Amazon SES publishes reputation metrics for the
-- configuration set, such as bounce and complaint rates, to Amazon
-- CloudWatch.
--
-- If the value is @true@, reputation metrics are published. If the value
-- is @false@, reputation metrics are not published. The default value is
-- @false@.
reputationOptions_reputationMetricsEnabled :: Lens.Lens' ReputationOptions (Prelude.Maybe Prelude.Bool)
reputationOptions_reputationMetricsEnabled = Lens.lens (\ReputationOptions' {reputationMetricsEnabled} -> reputationMetricsEnabled) (\s@ReputationOptions' {} a -> s {reputationMetricsEnabled = a} :: ReputationOptions)

-- | Describes whether email sending is enabled or disabled for the
-- configuration set. If the value is @true@, then Amazon SES will send
-- emails that use the configuration set. If the value is @false@, Amazon
-- SES will not send emails that use the configuration set. The default
-- value is @true@. You can change this setting using
-- UpdateConfigurationSetSendingEnabled.
reputationOptions_sendingEnabled :: Lens.Lens' ReputationOptions (Prelude.Maybe Prelude.Bool)
reputationOptions_sendingEnabled = Lens.lens (\ReputationOptions' {sendingEnabled} -> sendingEnabled) (\s@ReputationOptions' {} a -> s {sendingEnabled = a} :: ReputationOptions)

instance Data.FromXML ReputationOptions where
  parseXML x =
    ReputationOptions'
      Prelude.<$> (x Data..@? "LastFreshStart")
      Prelude.<*> (x Data..@? "ReputationMetricsEnabled")
      Prelude.<*> (x Data..@? "SendingEnabled")

instance Prelude.Hashable ReputationOptions where
  hashWithSalt _salt ReputationOptions' {..} =
    _salt
      `Prelude.hashWithSalt` lastFreshStart
      `Prelude.hashWithSalt` reputationMetricsEnabled
      `Prelude.hashWithSalt` sendingEnabled

instance Prelude.NFData ReputationOptions where
  rnf ReputationOptions' {..} =
    Prelude.rnf lastFreshStart `Prelude.seq`
      Prelude.rnf reputationMetricsEnabled `Prelude.seq`
        Prelude.rnf sendingEnabled
