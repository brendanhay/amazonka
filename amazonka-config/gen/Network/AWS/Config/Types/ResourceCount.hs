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
-- Module      : Network.AWS.Config.Types.ResourceCount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceCount where

import Network.AWS.Config.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that contains the resource type and the number of resources.
--
-- /See:/ 'newResourceCount' smart constructor.
data ResourceCount = ResourceCount'
  { -- | The resource type (for example, @\"AWS::EC2::Instance\"@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The number of resources.
    count :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'resourceCount_resourceType' - The resource type (for example, @\"AWS::EC2::Instance\"@).
--
-- 'count', 'resourceCount_count' - The number of resources.
newResourceCount ::
  ResourceCount
newResourceCount =
  ResourceCount'
    { resourceType = Prelude.Nothing,
      count = Prelude.Nothing
    }

-- | The resource type (for example, @\"AWS::EC2::Instance\"@).
resourceCount_resourceType :: Lens.Lens' ResourceCount (Prelude.Maybe ResourceType)
resourceCount_resourceType = Lens.lens (\ResourceCount' {resourceType} -> resourceType) (\s@ResourceCount' {} a -> s {resourceType = a} :: ResourceCount)

-- | The number of resources.
resourceCount_count :: Lens.Lens' ResourceCount (Prelude.Maybe Prelude.Integer)
resourceCount_count = Lens.lens (\ResourceCount' {count} -> count) (\s@ResourceCount' {} a -> s {count = a} :: ResourceCount)

instance Prelude.FromJSON ResourceCount where
  parseJSON =
    Prelude.withObject
      "ResourceCount"
      ( \x ->
          ResourceCount'
            Prelude.<$> (x Prelude..:? "resourceType")
            Prelude.<*> (x Prelude..:? "count")
      )

instance Prelude.Hashable ResourceCount

instance Prelude.NFData ResourceCount
