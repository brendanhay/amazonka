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
-- Module      : Amazonka.IAM.Types.AccessKeyLastUsed
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.AccessKeyLastUsed where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the last time an Amazon Web Services access
-- key was used since IAM began tracking this information on April 22,
-- 2015.
--
-- This data type is used as a response element in the GetAccessKeyLastUsed
-- operation.
--
-- /See:/ 'newAccessKeyLastUsed' smart constructor.
data AccessKeyLastUsed = AccessKeyLastUsed'
  { -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- access key was most recently used. This field is null in the following
    -- situations:
    --
    -- -   The user does not have an access key.
    --
    -- -   An access key exists but has not been used since IAM began tracking
    --     this information.
    --
    -- -   There is no sign-in data associated with the user.
    lastUsedDate :: Data.ISO8601,
    -- | The name of the Amazon Web Services service with which this access key
    -- was most recently used. The value of this field is \"N\/A\" in the
    -- following situations:
    --
    -- -   The user does not have an access key.
    --
    -- -   An access key exists but has not been used since IAM started
    --     tracking this information.
    --
    -- -   There is no sign-in data associated with the user.
    serviceName :: Prelude.Text,
    -- | The Amazon Web Services Region where this access key was most recently
    -- used. The value for this field is \"N\/A\" in the following situations:
    --
    -- -   The user does not have an access key.
    --
    -- -   An access key exists but has not been used since IAM began tracking
    --     this information.
    --
    -- -   There is no sign-in data associated with the user.
    --
    -- For more information about Amazon Web Services Regions, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and endpoints>
    -- in the Amazon Web Services General Reference.
    region :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessKeyLastUsed' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUsedDate', 'accessKeyLastUsed_lastUsedDate' - The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- access key was most recently used. This field is null in the following
-- situations:
--
-- -   The user does not have an access key.
--
-- -   An access key exists but has not been used since IAM began tracking
--     this information.
--
-- -   There is no sign-in data associated with the user.
--
-- 'serviceName', 'accessKeyLastUsed_serviceName' - The name of the Amazon Web Services service with which this access key
-- was most recently used. The value of this field is \"N\/A\" in the
-- following situations:
--
-- -   The user does not have an access key.
--
-- -   An access key exists but has not been used since IAM started
--     tracking this information.
--
-- -   There is no sign-in data associated with the user.
--
-- 'region', 'accessKeyLastUsed_region' - The Amazon Web Services Region where this access key was most recently
-- used. The value for this field is \"N\/A\" in the following situations:
--
-- -   The user does not have an access key.
--
-- -   An access key exists but has not been used since IAM began tracking
--     this information.
--
-- -   There is no sign-in data associated with the user.
--
-- For more information about Amazon Web Services Regions, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and endpoints>
-- in the Amazon Web Services General Reference.
newAccessKeyLastUsed ::
  -- | 'lastUsedDate'
  Prelude.UTCTime ->
  -- | 'serviceName'
  Prelude.Text ->
  -- | 'region'
  Prelude.Text ->
  AccessKeyLastUsed
newAccessKeyLastUsed
  pLastUsedDate_
  pServiceName_
  pRegion_ =
    AccessKeyLastUsed'
      { lastUsedDate =
          Data._Time Lens.# pLastUsedDate_,
        serviceName = pServiceName_,
        region = pRegion_
      }

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- access key was most recently used. This field is null in the following
-- situations:
--
-- -   The user does not have an access key.
--
-- -   An access key exists but has not been used since IAM began tracking
--     this information.
--
-- -   There is no sign-in data associated with the user.
accessKeyLastUsed_lastUsedDate :: Lens.Lens' AccessKeyLastUsed Prelude.UTCTime
accessKeyLastUsed_lastUsedDate = Lens.lens (\AccessKeyLastUsed' {lastUsedDate} -> lastUsedDate) (\s@AccessKeyLastUsed' {} a -> s {lastUsedDate = a} :: AccessKeyLastUsed) Prelude.. Data._Time

-- | The name of the Amazon Web Services service with which this access key
-- was most recently used. The value of this field is \"N\/A\" in the
-- following situations:
--
-- -   The user does not have an access key.
--
-- -   An access key exists but has not been used since IAM started
--     tracking this information.
--
-- -   There is no sign-in data associated with the user.
accessKeyLastUsed_serviceName :: Lens.Lens' AccessKeyLastUsed Prelude.Text
accessKeyLastUsed_serviceName = Lens.lens (\AccessKeyLastUsed' {serviceName} -> serviceName) (\s@AccessKeyLastUsed' {} a -> s {serviceName = a} :: AccessKeyLastUsed)

-- | The Amazon Web Services Region where this access key was most recently
-- used. The value for this field is \"N\/A\" in the following situations:
--
-- -   The user does not have an access key.
--
-- -   An access key exists but has not been used since IAM began tracking
--     this information.
--
-- -   There is no sign-in data associated with the user.
--
-- For more information about Amazon Web Services Regions, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and endpoints>
-- in the Amazon Web Services General Reference.
accessKeyLastUsed_region :: Lens.Lens' AccessKeyLastUsed Prelude.Text
accessKeyLastUsed_region = Lens.lens (\AccessKeyLastUsed' {region} -> region) (\s@AccessKeyLastUsed' {} a -> s {region = a} :: AccessKeyLastUsed)

instance Data.FromXML AccessKeyLastUsed where
  parseXML x =
    AccessKeyLastUsed'
      Prelude.<$> (x Data..@ "LastUsedDate")
      Prelude.<*> (x Data..@ "ServiceName")
      Prelude.<*> (x Data..@ "Region")

instance Prelude.Hashable AccessKeyLastUsed where
  hashWithSalt _salt AccessKeyLastUsed' {..} =
    _salt
      `Prelude.hashWithSalt` lastUsedDate
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` region

instance Prelude.NFData AccessKeyLastUsed where
  rnf AccessKeyLastUsed' {..} =
    Prelude.rnf lastUsedDate
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf region
