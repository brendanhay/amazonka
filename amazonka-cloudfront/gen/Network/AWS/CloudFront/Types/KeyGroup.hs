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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A key group.
--
-- A key group contains a list of public keys that you can use with
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html CloudFront signed URLs and signed cookies>.
--
-- /See:/ 'newKeyGroup' smart constructor.
data KeyGroup = KeyGroup'
  { -- | The identifier for the key group.
    id :: Core.Text,
    -- | The date and time when the key group was last modified.
    lastModifiedTime :: Core.ISO8601,
    -- | The key group configuration.
    keyGroupConfig :: KeyGroupConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'lastModifiedTime'
  Core.UTCTime ->
  -- | 'keyGroupConfig'
  KeyGroupConfig ->
  KeyGroup
newKeyGroup pId_ pLastModifiedTime_ pKeyGroupConfig_ =
  KeyGroup'
    { id = pId_,
      lastModifiedTime =
        Core._Time Lens.# pLastModifiedTime_,
      keyGroupConfig = pKeyGroupConfig_
    }

-- | The identifier for the key group.
keyGroup_id :: Lens.Lens' KeyGroup Core.Text
keyGroup_id = Lens.lens (\KeyGroup' {id} -> id) (\s@KeyGroup' {} a -> s {id = a} :: KeyGroup)

-- | The date and time when the key group was last modified.
keyGroup_lastModifiedTime :: Lens.Lens' KeyGroup Core.UTCTime
keyGroup_lastModifiedTime = Lens.lens (\KeyGroup' {lastModifiedTime} -> lastModifiedTime) (\s@KeyGroup' {} a -> s {lastModifiedTime = a} :: KeyGroup) Core.. Core._Time

-- | The key group configuration.
keyGroup_keyGroupConfig :: Lens.Lens' KeyGroup KeyGroupConfig
keyGroup_keyGroupConfig = Lens.lens (\KeyGroup' {keyGroupConfig} -> keyGroupConfig) (\s@KeyGroup' {} a -> s {keyGroupConfig = a} :: KeyGroup)

instance Core.FromXML KeyGroup where
  parseXML x =
    KeyGroup'
      Core.<$> (x Core..@ "Id")
      Core.<*> (x Core..@ "LastModifiedTime")
      Core.<*> (x Core..@ "KeyGroupConfig")

instance Core.Hashable KeyGroup

instance Core.NFData KeyGroup
