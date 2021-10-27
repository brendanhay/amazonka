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
-- Module      : Network.AWS.LakeFormation.Types.ResourceInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LakeFormation.Types.ResourceInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A structure containing information about an AWS Lake Formation resource.
--
-- /See:/ 'newResourceInfo' smart constructor.
data ResourceInfo = ResourceInfo'
  { -- | The Amazon Resource Name (ARN) of the resource.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time the resource was last modified.
    lastModified :: Prelude.Maybe Core.POSIX,
    -- | The IAM role that registered a resource.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'resourceInfo_resourceArn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'lastModified', 'resourceInfo_lastModified' - The date and time the resource was last modified.
--
-- 'roleArn', 'resourceInfo_roleArn' - The IAM role that registered a resource.
newResourceInfo ::
  ResourceInfo
newResourceInfo =
  ResourceInfo'
    { resourceArn = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the resource.
resourceInfo_resourceArn :: Lens.Lens' ResourceInfo (Prelude.Maybe Prelude.Text)
resourceInfo_resourceArn = Lens.lens (\ResourceInfo' {resourceArn} -> resourceArn) (\s@ResourceInfo' {} a -> s {resourceArn = a} :: ResourceInfo)

-- | The date and time the resource was last modified.
resourceInfo_lastModified :: Lens.Lens' ResourceInfo (Prelude.Maybe Prelude.UTCTime)
resourceInfo_lastModified = Lens.lens (\ResourceInfo' {lastModified} -> lastModified) (\s@ResourceInfo' {} a -> s {lastModified = a} :: ResourceInfo) Prelude.. Lens.mapping Core._Time

-- | The IAM role that registered a resource.
resourceInfo_roleArn :: Lens.Lens' ResourceInfo (Prelude.Maybe Prelude.Text)
resourceInfo_roleArn = Lens.lens (\ResourceInfo' {roleArn} -> roleArn) (\s@ResourceInfo' {} a -> s {roleArn = a} :: ResourceInfo)

instance Core.FromJSON ResourceInfo where
  parseJSON =
    Core.withObject
      "ResourceInfo"
      ( \x ->
          ResourceInfo'
            Prelude.<$> (x Core..:? "ResourceArn")
            Prelude.<*> (x Core..:? "LastModified")
            Prelude.<*> (x Core..:? "RoleArn")
      )

instance Prelude.Hashable ResourceInfo

instance Prelude.NFData ResourceInfo
