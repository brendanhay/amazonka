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
-- Module      : Network.AWS.SecurityHub.Types.AwsRedshiftClusterDeferredMaintenanceWindow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsRedshiftClusterDeferredMaintenanceWindow where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A time windows during which maintenance was deferred for an Amazon
-- Redshift cluster.
--
-- /See:/ 'newAwsRedshiftClusterDeferredMaintenanceWindow' smart constructor.
data AwsRedshiftClusterDeferredMaintenanceWindow = AwsRedshiftClusterDeferredMaintenanceWindow'
  { -- | The end of the time window for which maintenance was deferred.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    deferMaintenanceEndTime :: Prelude.Maybe Prelude.Text,
    -- | The start of the time window for which maintenance was deferred.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    deferMaintenanceStartTime :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the maintenance window.
    deferMaintenanceIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRedshiftClusterDeferredMaintenanceWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deferMaintenanceEndTime', 'awsRedshiftClusterDeferredMaintenanceWindow_deferMaintenanceEndTime' - The end of the time window for which maintenance was deferred.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'deferMaintenanceStartTime', 'awsRedshiftClusterDeferredMaintenanceWindow_deferMaintenanceStartTime' - The start of the time window for which maintenance was deferred.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'deferMaintenanceIdentifier', 'awsRedshiftClusterDeferredMaintenanceWindow_deferMaintenanceIdentifier' - The identifier of the maintenance window.
newAwsRedshiftClusterDeferredMaintenanceWindow ::
  AwsRedshiftClusterDeferredMaintenanceWindow
newAwsRedshiftClusterDeferredMaintenanceWindow =
  AwsRedshiftClusterDeferredMaintenanceWindow'
    { deferMaintenanceEndTime =
        Prelude.Nothing,
      deferMaintenanceStartTime =
        Prelude.Nothing,
      deferMaintenanceIdentifier =
        Prelude.Nothing
    }

-- | The end of the time window for which maintenance was deferred.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsRedshiftClusterDeferredMaintenanceWindow_deferMaintenanceEndTime :: Lens.Lens' AwsRedshiftClusterDeferredMaintenanceWindow (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDeferredMaintenanceWindow_deferMaintenanceEndTime = Lens.lens (\AwsRedshiftClusterDeferredMaintenanceWindow' {deferMaintenanceEndTime} -> deferMaintenanceEndTime) (\s@AwsRedshiftClusterDeferredMaintenanceWindow' {} a -> s {deferMaintenanceEndTime = a} :: AwsRedshiftClusterDeferredMaintenanceWindow)

-- | The start of the time window for which maintenance was deferred.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsRedshiftClusterDeferredMaintenanceWindow_deferMaintenanceStartTime :: Lens.Lens' AwsRedshiftClusterDeferredMaintenanceWindow (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDeferredMaintenanceWindow_deferMaintenanceStartTime = Lens.lens (\AwsRedshiftClusterDeferredMaintenanceWindow' {deferMaintenanceStartTime} -> deferMaintenanceStartTime) (\s@AwsRedshiftClusterDeferredMaintenanceWindow' {} a -> s {deferMaintenanceStartTime = a} :: AwsRedshiftClusterDeferredMaintenanceWindow)

-- | The identifier of the maintenance window.
awsRedshiftClusterDeferredMaintenanceWindow_deferMaintenanceIdentifier :: Lens.Lens' AwsRedshiftClusterDeferredMaintenanceWindow (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDeferredMaintenanceWindow_deferMaintenanceIdentifier = Lens.lens (\AwsRedshiftClusterDeferredMaintenanceWindow' {deferMaintenanceIdentifier} -> deferMaintenanceIdentifier) (\s@AwsRedshiftClusterDeferredMaintenanceWindow' {} a -> s {deferMaintenanceIdentifier = a} :: AwsRedshiftClusterDeferredMaintenanceWindow)

instance
  Core.FromJSON
    AwsRedshiftClusterDeferredMaintenanceWindow
  where
  parseJSON =
    Core.withObject
      "AwsRedshiftClusterDeferredMaintenanceWindow"
      ( \x ->
          AwsRedshiftClusterDeferredMaintenanceWindow'
            Prelude.<$> (x Core..:? "DeferMaintenanceEndTime")
              Prelude.<*> (x Core..:? "DeferMaintenanceStartTime")
              Prelude.<*> (x Core..:? "DeferMaintenanceIdentifier")
      )

instance
  Prelude.Hashable
    AwsRedshiftClusterDeferredMaintenanceWindow

instance
  Prelude.NFData
    AwsRedshiftClusterDeferredMaintenanceWindow

instance
  Core.ToJSON
    AwsRedshiftClusterDeferredMaintenanceWindow
  where
  toJSON
    AwsRedshiftClusterDeferredMaintenanceWindow' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("DeferMaintenanceEndTime" Core..=)
                Prelude.<$> deferMaintenanceEndTime,
              ("DeferMaintenanceStartTime" Core..=)
                Prelude.<$> deferMaintenanceStartTime,
              ("DeferMaintenanceIdentifier" Core..=)
                Prelude.<$> deferMaintenanceIdentifier
            ]
        )
