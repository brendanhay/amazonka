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
-- Module      : Amazonka.LakeFormation.Types.ResourceInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.ResourceInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure containing information about an Lake Formation resource.
--
-- /See:/ 'newResourceInfo' smart constructor.
data ResourceInfo = ResourceInfo'
  { -- | The date and time the resource was last modified.
    lastModified :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the resource.
    resourceArn :: Prelude.Maybe Prelude.Text,
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
-- 'lastModified', 'resourceInfo_lastModified' - The date and time the resource was last modified.
--
-- 'resourceArn', 'resourceInfo_resourceArn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'roleArn', 'resourceInfo_roleArn' - The IAM role that registered a resource.
newResourceInfo ::
  ResourceInfo
newResourceInfo =
  ResourceInfo'
    { lastModified = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | The date and time the resource was last modified.
resourceInfo_lastModified :: Lens.Lens' ResourceInfo (Prelude.Maybe Prelude.UTCTime)
resourceInfo_lastModified = Lens.lens (\ResourceInfo' {lastModified} -> lastModified) (\s@ResourceInfo' {} a -> s {lastModified = a} :: ResourceInfo) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the resource.
resourceInfo_resourceArn :: Lens.Lens' ResourceInfo (Prelude.Maybe Prelude.Text)
resourceInfo_resourceArn = Lens.lens (\ResourceInfo' {resourceArn} -> resourceArn) (\s@ResourceInfo' {} a -> s {resourceArn = a} :: ResourceInfo)

-- | The IAM role that registered a resource.
resourceInfo_roleArn :: Lens.Lens' ResourceInfo (Prelude.Maybe Prelude.Text)
resourceInfo_roleArn = Lens.lens (\ResourceInfo' {roleArn} -> roleArn) (\s@ResourceInfo' {} a -> s {roleArn = a} :: ResourceInfo)

instance Data.FromJSON ResourceInfo where
  parseJSON =
    Data.withObject
      "ResourceInfo"
      ( \x ->
          ResourceInfo'
            Prelude.<$> (x Data..:? "LastModified")
            Prelude.<*> (x Data..:? "ResourceArn")
            Prelude.<*> (x Data..:? "RoleArn")
      )

instance Prelude.Hashable ResourceInfo where
  hashWithSalt _salt ResourceInfo' {..} =
    _salt
      `Prelude.hashWithSalt` lastModified
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData ResourceInfo where
  rnf ResourceInfo' {..} =
    Prelude.rnf lastModified `Prelude.seq`
      Prelude.rnf resourceArn `Prelude.seq`
        Prelude.rnf roleArn
