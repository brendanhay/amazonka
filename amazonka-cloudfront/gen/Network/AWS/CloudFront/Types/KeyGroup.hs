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
-- Module      : Network.AWS.CloudFront.Types.KeyGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.KeyGroup where

import Network.AWS.CloudFront.Types.KeyGroupConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A key group.
--
-- A key group contains a list of public keys that you can use with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html CloudFront signed URLs and signed cookies>.
--
-- /See:/ 'newKeyGroup' smart constructor.
data KeyGroup = KeyGroup'
  { -- | The identifier for the key group.
    id :: Prelude.Text,
    -- | The date and time when the key group was last modified.
    lastModifiedTime :: Prelude.ISO8601,
    -- | The key group configuration.
    keyGroupConfig :: KeyGroupConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KeyGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'keyGroup_id' - The identifier for the key group.
--
-- 'lastModifiedTime', 'keyGroup_lastModifiedTime' - The date and time when the key group was last modified.
--
-- 'keyGroupConfig', 'keyGroup_keyGroupConfig' - The key group configuration.
newKeyGroup ::
  -- | 'id'
  Prelude.Text ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'keyGroupConfig'
  KeyGroupConfig ->
  KeyGroup
newKeyGroup pId_ pLastModifiedTime_ pKeyGroupConfig_ =
  KeyGroup'
    { id = pId_,
      lastModifiedTime =
        Prelude._Time Lens.# pLastModifiedTime_,
      keyGroupConfig = pKeyGroupConfig_
    }

-- | The identifier for the key group.
keyGroup_id :: Lens.Lens' KeyGroup Prelude.Text
keyGroup_id = Lens.lens (\KeyGroup' {id} -> id) (\s@KeyGroup' {} a -> s {id = a} :: KeyGroup)

-- | The date and time when the key group was last modified.
keyGroup_lastModifiedTime :: Lens.Lens' KeyGroup Prelude.UTCTime
keyGroup_lastModifiedTime = Lens.lens (\KeyGroup' {lastModifiedTime} -> lastModifiedTime) (\s@KeyGroup' {} a -> s {lastModifiedTime = a} :: KeyGroup) Prelude.. Prelude._Time

-- | The key group configuration.
keyGroup_keyGroupConfig :: Lens.Lens' KeyGroup KeyGroupConfig
keyGroup_keyGroupConfig = Lens.lens (\KeyGroup' {keyGroupConfig} -> keyGroupConfig) (\s@KeyGroup' {} a -> s {keyGroupConfig = a} :: KeyGroup)

instance Prelude.FromXML KeyGroup where
  parseXML x =
    KeyGroup'
      Prelude.<$> (x Prelude..@ "Id")
      Prelude.<*> (x Prelude..@ "LastModifiedTime")
      Prelude.<*> (x Prelude..@ "KeyGroupConfig")

instance Prelude.Hashable KeyGroup

instance Prelude.NFData KeyGroup
