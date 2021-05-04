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
-- Module      : Network.AWS.EC2.Types.TagDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TagDescription where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a tag.
--
-- /See:/ 'newTagDescription' smart constructor.
data TagDescription = TagDescription'
  { -- | The ID of the resource.
    resourceId :: Prelude.Text,
    -- | The resource type.
    resourceType :: ResourceType,
    -- | The tag key.
    key :: Prelude.Text,
    -- | The tag value.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TagDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'tagDescription_resourceId' - The ID of the resource.
--
-- 'resourceType', 'tagDescription_resourceType' - The resource type.
--
-- 'key', 'tagDescription_key' - The tag key.
--
-- 'value', 'tagDescription_value' - The tag value.
newTagDescription ::
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'resourceType'
  ResourceType ->
  -- | 'key'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  TagDescription
newTagDescription
  pResourceId_
  pResourceType_
  pKey_
  pValue_ =
    TagDescription'
      { resourceId = pResourceId_,
        resourceType = pResourceType_,
        key = pKey_,
        value = pValue_
      }

-- | The ID of the resource.
tagDescription_resourceId :: Lens.Lens' TagDescription Prelude.Text
tagDescription_resourceId = Lens.lens (\TagDescription' {resourceId} -> resourceId) (\s@TagDescription' {} a -> s {resourceId = a} :: TagDescription)

-- | The resource type.
tagDescription_resourceType :: Lens.Lens' TagDescription ResourceType
tagDescription_resourceType = Lens.lens (\TagDescription' {resourceType} -> resourceType) (\s@TagDescription' {} a -> s {resourceType = a} :: TagDescription)

-- | The tag key.
tagDescription_key :: Lens.Lens' TagDescription Prelude.Text
tagDescription_key = Lens.lens (\TagDescription' {key} -> key) (\s@TagDescription' {} a -> s {key = a} :: TagDescription)

-- | The tag value.
tagDescription_value :: Lens.Lens' TagDescription Prelude.Text
tagDescription_value = Lens.lens (\TagDescription' {value} -> value) (\s@TagDescription' {} a -> s {value = a} :: TagDescription)

instance Prelude.FromXML TagDescription where
  parseXML x =
    TagDescription'
      Prelude.<$> (x Prelude..@ "resourceId")
      Prelude.<*> (x Prelude..@ "resourceType")
      Prelude.<*> (x Prelude..@ "key")
      Prelude.<*> (x Prelude..@ "value")

instance Prelude.Hashable TagDescription

instance Prelude.NFData TagDescription
