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
-- Module      : Amazonka.VPCLattice.Types.AccessLogSubscriptionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.AccessLogSubscriptionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary information about an access log subscription.
--
-- /See:/ 'newAccessLogSubscriptionSummary' smart constructor.
data AccessLogSubscriptionSummary = AccessLogSubscriptionSummary'
  { -- | The Amazon Resource Name (ARN) of the access log subscription
    arn :: Prelude.Text,
    -- | The date and time that the access log subscription was created,
    -- specified in ISO-8601 format.
    createdAt :: Data.ISO8601,
    -- | The Amazon Resource Name (ARN) of the destination.
    destinationArn :: Prelude.Text,
    -- | The ID of the access log subscription.
    id :: Prelude.Text,
    -- | The date and time that the access log subscription was last updated,
    -- specified in ISO-8601 format.
    lastUpdatedAt :: Data.ISO8601,
    -- | The Amazon Resource Name (ARN) of the service or service network.
    resourceArn :: Prelude.Text,
    -- | The ID of the service or service network.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessLogSubscriptionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'accessLogSubscriptionSummary_arn' - The Amazon Resource Name (ARN) of the access log subscription
--
-- 'createdAt', 'accessLogSubscriptionSummary_createdAt' - The date and time that the access log subscription was created,
-- specified in ISO-8601 format.
--
-- 'destinationArn', 'accessLogSubscriptionSummary_destinationArn' - The Amazon Resource Name (ARN) of the destination.
--
-- 'id', 'accessLogSubscriptionSummary_id' - The ID of the access log subscription.
--
-- 'lastUpdatedAt', 'accessLogSubscriptionSummary_lastUpdatedAt' - The date and time that the access log subscription was last updated,
-- specified in ISO-8601 format.
--
-- 'resourceArn', 'accessLogSubscriptionSummary_resourceArn' - The Amazon Resource Name (ARN) of the service or service network.
--
-- 'resourceId', 'accessLogSubscriptionSummary_resourceId' - The ID of the service or service network.
newAccessLogSubscriptionSummary ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'destinationArn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'lastUpdatedAt'
  Prelude.UTCTime ->
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  AccessLogSubscriptionSummary
newAccessLogSubscriptionSummary
  pArn_
  pCreatedAt_
  pDestinationArn_
  pId_
  pLastUpdatedAt_
  pResourceArn_
  pResourceId_ =
    AccessLogSubscriptionSummary'
      { arn = pArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        destinationArn = pDestinationArn_,
        id = pId_,
        lastUpdatedAt =
          Data._Time Lens.# pLastUpdatedAt_,
        resourceArn = pResourceArn_,
        resourceId = pResourceId_
      }

-- | The Amazon Resource Name (ARN) of the access log subscription
accessLogSubscriptionSummary_arn :: Lens.Lens' AccessLogSubscriptionSummary Prelude.Text
accessLogSubscriptionSummary_arn = Lens.lens (\AccessLogSubscriptionSummary' {arn} -> arn) (\s@AccessLogSubscriptionSummary' {} a -> s {arn = a} :: AccessLogSubscriptionSummary)

-- | The date and time that the access log subscription was created,
-- specified in ISO-8601 format.
accessLogSubscriptionSummary_createdAt :: Lens.Lens' AccessLogSubscriptionSummary Prelude.UTCTime
accessLogSubscriptionSummary_createdAt = Lens.lens (\AccessLogSubscriptionSummary' {createdAt} -> createdAt) (\s@AccessLogSubscriptionSummary' {} a -> s {createdAt = a} :: AccessLogSubscriptionSummary) Prelude.. Data._Time

-- | The Amazon Resource Name (ARN) of the destination.
accessLogSubscriptionSummary_destinationArn :: Lens.Lens' AccessLogSubscriptionSummary Prelude.Text
accessLogSubscriptionSummary_destinationArn = Lens.lens (\AccessLogSubscriptionSummary' {destinationArn} -> destinationArn) (\s@AccessLogSubscriptionSummary' {} a -> s {destinationArn = a} :: AccessLogSubscriptionSummary)

-- | The ID of the access log subscription.
accessLogSubscriptionSummary_id :: Lens.Lens' AccessLogSubscriptionSummary Prelude.Text
accessLogSubscriptionSummary_id = Lens.lens (\AccessLogSubscriptionSummary' {id} -> id) (\s@AccessLogSubscriptionSummary' {} a -> s {id = a} :: AccessLogSubscriptionSummary)

-- | The date and time that the access log subscription was last updated,
-- specified in ISO-8601 format.
accessLogSubscriptionSummary_lastUpdatedAt :: Lens.Lens' AccessLogSubscriptionSummary Prelude.UTCTime
accessLogSubscriptionSummary_lastUpdatedAt = Lens.lens (\AccessLogSubscriptionSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@AccessLogSubscriptionSummary' {} a -> s {lastUpdatedAt = a} :: AccessLogSubscriptionSummary) Prelude.. Data._Time

-- | The Amazon Resource Name (ARN) of the service or service network.
accessLogSubscriptionSummary_resourceArn :: Lens.Lens' AccessLogSubscriptionSummary Prelude.Text
accessLogSubscriptionSummary_resourceArn = Lens.lens (\AccessLogSubscriptionSummary' {resourceArn} -> resourceArn) (\s@AccessLogSubscriptionSummary' {} a -> s {resourceArn = a} :: AccessLogSubscriptionSummary)

-- | The ID of the service or service network.
accessLogSubscriptionSummary_resourceId :: Lens.Lens' AccessLogSubscriptionSummary Prelude.Text
accessLogSubscriptionSummary_resourceId = Lens.lens (\AccessLogSubscriptionSummary' {resourceId} -> resourceId) (\s@AccessLogSubscriptionSummary' {} a -> s {resourceId = a} :: AccessLogSubscriptionSummary)

instance Data.FromJSON AccessLogSubscriptionSummary where
  parseJSON =
    Data.withObject
      "AccessLogSubscriptionSummary"
      ( \x ->
          AccessLogSubscriptionSummary'
            Prelude.<$> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "destinationArn")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "lastUpdatedAt")
            Prelude.<*> (x Data..: "resourceArn")
            Prelude.<*> (x Data..: "resourceId")
      )

instance
  Prelude.Hashable
    AccessLogSubscriptionSummary
  where
  hashWithSalt _salt AccessLogSubscriptionSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` destinationArn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData AccessLogSubscriptionSummary where
  rnf AccessLogSubscriptionSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf destinationArn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourceId
