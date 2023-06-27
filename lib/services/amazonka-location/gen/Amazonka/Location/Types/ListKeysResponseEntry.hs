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
-- Module      : Amazonka.Location.Types.ListKeysResponseEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.ListKeysResponseEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types.ApiKeyRestrictions
import qualified Amazonka.Prelude as Prelude

-- | An API key resource listed in your Amazon Web Services account.
--
-- /See:/ 'newListKeysResponseEntry' smart constructor.
data ListKeysResponseEntry = ListKeysResponseEntry'
  { -- | The optional description for the API key resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the API key was created, in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    createTime :: Data.ISO8601,
    -- | The timestamp for when the API key resource will expire, in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    expireTime :: Data.ISO8601,
    -- | The name of the API key resource.
    keyName :: Prelude.Text,
    restrictions :: ApiKeyRestrictions,
    -- | The timestamp of when the API key was last updated, in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    updateTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKeysResponseEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'listKeysResponseEntry_description' - The optional description for the API key resource.
--
-- 'createTime', 'listKeysResponseEntry_createTime' - The timestamp of when the API key was created, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- 'expireTime', 'listKeysResponseEntry_expireTime' - The timestamp for when the API key resource will expire, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- 'keyName', 'listKeysResponseEntry_keyName' - The name of the API key resource.
--
-- 'restrictions', 'listKeysResponseEntry_restrictions' - Undocumented member.
--
-- 'updateTime', 'listKeysResponseEntry_updateTime' - The timestamp of when the API key was last updated, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
newListKeysResponseEntry ::
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'expireTime'
  Prelude.UTCTime ->
  -- | 'keyName'
  Prelude.Text ->
  -- | 'restrictions'
  ApiKeyRestrictions ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  ListKeysResponseEntry
newListKeysResponseEntry
  pCreateTime_
  pExpireTime_
  pKeyName_
  pRestrictions_
  pUpdateTime_ =
    ListKeysResponseEntry'
      { description =
          Prelude.Nothing,
        createTime = Data._Time Lens.# pCreateTime_,
        expireTime = Data._Time Lens.# pExpireTime_,
        keyName = pKeyName_,
        restrictions = pRestrictions_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | The optional description for the API key resource.
listKeysResponseEntry_description :: Lens.Lens' ListKeysResponseEntry (Prelude.Maybe Prelude.Text)
listKeysResponseEntry_description = Lens.lens (\ListKeysResponseEntry' {description} -> description) (\s@ListKeysResponseEntry' {} a -> s {description = a} :: ListKeysResponseEntry)

-- | The timestamp of when the API key was created, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
listKeysResponseEntry_createTime :: Lens.Lens' ListKeysResponseEntry Prelude.UTCTime
listKeysResponseEntry_createTime = Lens.lens (\ListKeysResponseEntry' {createTime} -> createTime) (\s@ListKeysResponseEntry' {} a -> s {createTime = a} :: ListKeysResponseEntry) Prelude.. Data._Time

-- | The timestamp for when the API key resource will expire, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
listKeysResponseEntry_expireTime :: Lens.Lens' ListKeysResponseEntry Prelude.UTCTime
listKeysResponseEntry_expireTime = Lens.lens (\ListKeysResponseEntry' {expireTime} -> expireTime) (\s@ListKeysResponseEntry' {} a -> s {expireTime = a} :: ListKeysResponseEntry) Prelude.. Data._Time

-- | The name of the API key resource.
listKeysResponseEntry_keyName :: Lens.Lens' ListKeysResponseEntry Prelude.Text
listKeysResponseEntry_keyName = Lens.lens (\ListKeysResponseEntry' {keyName} -> keyName) (\s@ListKeysResponseEntry' {} a -> s {keyName = a} :: ListKeysResponseEntry)

-- | Undocumented member.
listKeysResponseEntry_restrictions :: Lens.Lens' ListKeysResponseEntry ApiKeyRestrictions
listKeysResponseEntry_restrictions = Lens.lens (\ListKeysResponseEntry' {restrictions} -> restrictions) (\s@ListKeysResponseEntry' {} a -> s {restrictions = a} :: ListKeysResponseEntry)

-- | The timestamp of when the API key was last updated, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
listKeysResponseEntry_updateTime :: Lens.Lens' ListKeysResponseEntry Prelude.UTCTime
listKeysResponseEntry_updateTime = Lens.lens (\ListKeysResponseEntry' {updateTime} -> updateTime) (\s@ListKeysResponseEntry' {} a -> s {updateTime = a} :: ListKeysResponseEntry) Prelude.. Data._Time

instance Data.FromJSON ListKeysResponseEntry where
  parseJSON =
    Data.withObject
      "ListKeysResponseEntry"
      ( \x ->
          ListKeysResponseEntry'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..: "CreateTime")
            Prelude.<*> (x Data..: "ExpireTime")
            Prelude.<*> (x Data..: "KeyName")
            Prelude.<*> (x Data..: "Restrictions")
            Prelude.<*> (x Data..: "UpdateTime")
      )

instance Prelude.Hashable ListKeysResponseEntry where
  hashWithSalt _salt ListKeysResponseEntry' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` expireTime
      `Prelude.hashWithSalt` keyName
      `Prelude.hashWithSalt` restrictions
      `Prelude.hashWithSalt` updateTime

instance Prelude.NFData ListKeysResponseEntry where
  rnf ListKeysResponseEntry' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf expireTime
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf restrictions
      `Prelude.seq` Prelude.rnf updateTime
