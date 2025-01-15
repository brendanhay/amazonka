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
-- Module      : Amazonka.Lightsail.Types.AccountLevelBpaSync
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.AccountLevelBpaSync where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.AccountLevelBpaSyncStatus
import Amazonka.Lightsail.Types.BPAStatusMessage
import qualified Amazonka.Prelude as Prelude

-- | Describes the synchronization status of the Amazon Simple Storage
-- Service (Amazon S3) account-level block public access (BPA) feature for
-- your Lightsail buckets.
--
-- The account-level BPA feature of Amazon S3 provides centralized controls
-- to limit public access to all Amazon S3 buckets in an account. BPA can
-- make all Amazon S3 buckets in an Amazon Web Services account private
-- regardless of the individual bucket and object permissions that are
-- configured. Lightsail buckets take into account the Amazon S3
-- account-level BPA configuration when allowing or denying public access.
-- To do this, Lightsail periodically fetches the account-level BPA
-- configuration from Amazon S3. When the account-level BPA status is
-- @InSync@, the Amazon S3 account-level BPA configuration is synchronized
-- and it applies to your Lightsail buckets. For more information about
-- Amazon Simple Storage Service account-level BPA and how it affects
-- Lightsail buckets, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-block-public-access-for-buckets Block public access for buckets in Amazon Lightsail>
-- in the /Amazon Lightsail Developer Guide/.
--
-- /See:/ 'newAccountLevelBpaSync' smart constructor.
data AccountLevelBpaSync = AccountLevelBpaSync'
  { -- | A Boolean value that indicates whether account-level block public access
    -- is affecting your Lightsail buckets.
    bpaImpactsLightsail :: Prelude.Maybe Prelude.Bool,
    -- | The timestamp of when the account-level BPA configuration was last
    -- synchronized. This value is null when the account-level BPA
    -- configuration has not been synchronized.
    lastSyncedAt :: Prelude.Maybe Data.POSIX,
    -- | A message that provides a reason for a @Failed@ or @Defaulted@
    -- synchronization status.
    --
    -- The following messages are possible:
    --
    -- -   @SYNC_ON_HOLD@ - The synchronization has not yet happened. This
    --     status message occurs immediately after you create your first
    --     Lightsail bucket. This status message should change after the first
    --     synchronization happens, approximately 1 hour after the first bucket
    --     is created.
    --
    -- -   @DEFAULTED_FOR_SLR_MISSING@ - The synchronization failed because the
    --     required service-linked role is missing from your Amazon Web
    --     Services account. The account-level BPA configuration for your
    --     Lightsail buckets is defaulted to /active/ until the synchronization
    --     can occur. This means that all your buckets are private and not
    --     publicly accessible. For more information about how to create the
    --     required service-linked role to allow synchronization, see
    --     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-using-service-linked-roles Using Service-Linked Roles for Amazon Lightsail>
    --     in the /Amazon Lightsail Developer Guide/.
    --
    -- -   @DEFAULTED_FOR_SLR_MISSING_ON_HOLD@ - The synchronization failed
    --     because the required service-linked role is missing from your Amazon
    --     Web Services account. Account-level BPA is not yet configured for
    --     your Lightsail buckets. Therefore, only the bucket access
    --     permissions and individual object access permissions apply to your
    --     Lightsail buckets. For more information about how to create the
    --     required service-linked role to allow synchronization, see
    --     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-using-service-linked-roles Using Service-Linked Roles for Amazon Lightsail>
    --     in the /Amazon Lightsail Developer Guide/.
    --
    -- -   @Unknown@ - The reason that synchronization failed is unknown.
    --     Contact Amazon Web Services Support for more information.
    message :: Prelude.Maybe BPAStatusMessage,
    -- | The status of the account-level BPA synchronization.
    --
    -- The following statuses are possible:
    --
    -- -   @InSync@ - Account-level BPA is synchronized. The Amazon S3
    --     account-level BPA configuration applies to your Lightsail buckets.
    --
    -- -   @NeverSynced@ - Synchronization has not yet happened. The Amazon S3
    --     account-level BPA configuration does not apply to your Lightsail
    --     buckets.
    --
    -- -   @Failed@ - Synchronization failed. The Amazon S3 account-level BPA
    --     configuration does not apply to your Lightsail buckets.
    --
    -- -   @Defaulted@ - Synchronization failed and account-level BPA for your
    --     Lightsail buckets is defaulted to /active/.
    --
    -- You might need to complete further actions if the status is @Failed@ or
    -- @Defaulted@. The @message@ parameter provides more information for those
    -- statuses.
    status :: Prelude.Maybe AccountLevelBpaSyncStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountLevelBpaSync' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bpaImpactsLightsail', 'accountLevelBpaSync_bpaImpactsLightsail' - A Boolean value that indicates whether account-level block public access
-- is affecting your Lightsail buckets.
--
-- 'lastSyncedAt', 'accountLevelBpaSync_lastSyncedAt' - The timestamp of when the account-level BPA configuration was last
-- synchronized. This value is null when the account-level BPA
-- configuration has not been synchronized.
--
-- 'message', 'accountLevelBpaSync_message' - A message that provides a reason for a @Failed@ or @Defaulted@
-- synchronization status.
--
-- The following messages are possible:
--
-- -   @SYNC_ON_HOLD@ - The synchronization has not yet happened. This
--     status message occurs immediately after you create your first
--     Lightsail bucket. This status message should change after the first
--     synchronization happens, approximately 1 hour after the first bucket
--     is created.
--
-- -   @DEFAULTED_FOR_SLR_MISSING@ - The synchronization failed because the
--     required service-linked role is missing from your Amazon Web
--     Services account. The account-level BPA configuration for your
--     Lightsail buckets is defaulted to /active/ until the synchronization
--     can occur. This means that all your buckets are private and not
--     publicly accessible. For more information about how to create the
--     required service-linked role to allow synchronization, see
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-using-service-linked-roles Using Service-Linked Roles for Amazon Lightsail>
--     in the /Amazon Lightsail Developer Guide/.
--
-- -   @DEFAULTED_FOR_SLR_MISSING_ON_HOLD@ - The synchronization failed
--     because the required service-linked role is missing from your Amazon
--     Web Services account. Account-level BPA is not yet configured for
--     your Lightsail buckets. Therefore, only the bucket access
--     permissions and individual object access permissions apply to your
--     Lightsail buckets. For more information about how to create the
--     required service-linked role to allow synchronization, see
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-using-service-linked-roles Using Service-Linked Roles for Amazon Lightsail>
--     in the /Amazon Lightsail Developer Guide/.
--
-- -   @Unknown@ - The reason that synchronization failed is unknown.
--     Contact Amazon Web Services Support for more information.
--
-- 'status', 'accountLevelBpaSync_status' - The status of the account-level BPA synchronization.
--
-- The following statuses are possible:
--
-- -   @InSync@ - Account-level BPA is synchronized. The Amazon S3
--     account-level BPA configuration applies to your Lightsail buckets.
--
-- -   @NeverSynced@ - Synchronization has not yet happened. The Amazon S3
--     account-level BPA configuration does not apply to your Lightsail
--     buckets.
--
-- -   @Failed@ - Synchronization failed. The Amazon S3 account-level BPA
--     configuration does not apply to your Lightsail buckets.
--
-- -   @Defaulted@ - Synchronization failed and account-level BPA for your
--     Lightsail buckets is defaulted to /active/.
--
-- You might need to complete further actions if the status is @Failed@ or
-- @Defaulted@. The @message@ parameter provides more information for those
-- statuses.
newAccountLevelBpaSync ::
  AccountLevelBpaSync
newAccountLevelBpaSync =
  AccountLevelBpaSync'
    { bpaImpactsLightsail =
        Prelude.Nothing,
      lastSyncedAt = Prelude.Nothing,
      message = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | A Boolean value that indicates whether account-level block public access
-- is affecting your Lightsail buckets.
accountLevelBpaSync_bpaImpactsLightsail :: Lens.Lens' AccountLevelBpaSync (Prelude.Maybe Prelude.Bool)
accountLevelBpaSync_bpaImpactsLightsail = Lens.lens (\AccountLevelBpaSync' {bpaImpactsLightsail} -> bpaImpactsLightsail) (\s@AccountLevelBpaSync' {} a -> s {bpaImpactsLightsail = a} :: AccountLevelBpaSync)

-- | The timestamp of when the account-level BPA configuration was last
-- synchronized. This value is null when the account-level BPA
-- configuration has not been synchronized.
accountLevelBpaSync_lastSyncedAt :: Lens.Lens' AccountLevelBpaSync (Prelude.Maybe Prelude.UTCTime)
accountLevelBpaSync_lastSyncedAt = Lens.lens (\AccountLevelBpaSync' {lastSyncedAt} -> lastSyncedAt) (\s@AccountLevelBpaSync' {} a -> s {lastSyncedAt = a} :: AccountLevelBpaSync) Prelude.. Lens.mapping Data._Time

-- | A message that provides a reason for a @Failed@ or @Defaulted@
-- synchronization status.
--
-- The following messages are possible:
--
-- -   @SYNC_ON_HOLD@ - The synchronization has not yet happened. This
--     status message occurs immediately after you create your first
--     Lightsail bucket. This status message should change after the first
--     synchronization happens, approximately 1 hour after the first bucket
--     is created.
--
-- -   @DEFAULTED_FOR_SLR_MISSING@ - The synchronization failed because the
--     required service-linked role is missing from your Amazon Web
--     Services account. The account-level BPA configuration for your
--     Lightsail buckets is defaulted to /active/ until the synchronization
--     can occur. This means that all your buckets are private and not
--     publicly accessible. For more information about how to create the
--     required service-linked role to allow synchronization, see
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-using-service-linked-roles Using Service-Linked Roles for Amazon Lightsail>
--     in the /Amazon Lightsail Developer Guide/.
--
-- -   @DEFAULTED_FOR_SLR_MISSING_ON_HOLD@ - The synchronization failed
--     because the required service-linked role is missing from your Amazon
--     Web Services account. Account-level BPA is not yet configured for
--     your Lightsail buckets. Therefore, only the bucket access
--     permissions and individual object access permissions apply to your
--     Lightsail buckets. For more information about how to create the
--     required service-linked role to allow synchronization, see
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-using-service-linked-roles Using Service-Linked Roles for Amazon Lightsail>
--     in the /Amazon Lightsail Developer Guide/.
--
-- -   @Unknown@ - The reason that synchronization failed is unknown.
--     Contact Amazon Web Services Support for more information.
accountLevelBpaSync_message :: Lens.Lens' AccountLevelBpaSync (Prelude.Maybe BPAStatusMessage)
accountLevelBpaSync_message = Lens.lens (\AccountLevelBpaSync' {message} -> message) (\s@AccountLevelBpaSync' {} a -> s {message = a} :: AccountLevelBpaSync)

-- | The status of the account-level BPA synchronization.
--
-- The following statuses are possible:
--
-- -   @InSync@ - Account-level BPA is synchronized. The Amazon S3
--     account-level BPA configuration applies to your Lightsail buckets.
--
-- -   @NeverSynced@ - Synchronization has not yet happened. The Amazon S3
--     account-level BPA configuration does not apply to your Lightsail
--     buckets.
--
-- -   @Failed@ - Synchronization failed. The Amazon S3 account-level BPA
--     configuration does not apply to your Lightsail buckets.
--
-- -   @Defaulted@ - Synchronization failed and account-level BPA for your
--     Lightsail buckets is defaulted to /active/.
--
-- You might need to complete further actions if the status is @Failed@ or
-- @Defaulted@. The @message@ parameter provides more information for those
-- statuses.
accountLevelBpaSync_status :: Lens.Lens' AccountLevelBpaSync (Prelude.Maybe AccountLevelBpaSyncStatus)
accountLevelBpaSync_status = Lens.lens (\AccountLevelBpaSync' {status} -> status) (\s@AccountLevelBpaSync' {} a -> s {status = a} :: AccountLevelBpaSync)

instance Data.FromJSON AccountLevelBpaSync where
  parseJSON =
    Data.withObject
      "AccountLevelBpaSync"
      ( \x ->
          AccountLevelBpaSync'
            Prelude.<$> (x Data..:? "bpaImpactsLightsail")
            Prelude.<*> (x Data..:? "lastSyncedAt")
            Prelude.<*> (x Data..:? "message")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable AccountLevelBpaSync where
  hashWithSalt _salt AccountLevelBpaSync' {..} =
    _salt
      `Prelude.hashWithSalt` bpaImpactsLightsail
      `Prelude.hashWithSalt` lastSyncedAt
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` status

instance Prelude.NFData AccountLevelBpaSync where
  rnf AccountLevelBpaSync' {..} =
    Prelude.rnf bpaImpactsLightsail `Prelude.seq`
      Prelude.rnf lastSyncedAt `Prelude.seq`
        Prelude.rnf message `Prelude.seq`
          Prelude.rnf status
