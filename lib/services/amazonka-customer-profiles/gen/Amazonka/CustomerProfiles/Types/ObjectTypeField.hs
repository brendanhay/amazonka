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
-- Module      : Amazonka.CustomerProfiles.Types.ObjectTypeField
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.ObjectTypeField where

import qualified Amazonka.Core as Core
import Amazonka.CustomerProfiles.Types.FieldContentType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a field in a ProfileObjectType.
--
-- /See:/ 'newObjectTypeField' smart constructor.
data ObjectTypeField = ObjectTypeField'
  { -- | A field of a ProfileObject. For example: _source.FirstName, where
    -- “_source” is a ProfileObjectType of a Zendesk user and “FirstName” is a
    -- field in that ObjectType.
    source :: Prelude.Maybe Prelude.Text,
    -- | The content type of the field. Used for determining equality when
    -- searching.
    contentType :: Prelude.Maybe FieldContentType,
    -- | The location of the data in the standard ProfileObject model. For
    -- example: _profile.Address.PostalCode.
    target :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ObjectTypeField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'source', 'objectTypeField_source' - A field of a ProfileObject. For example: _source.FirstName, where
-- “_source” is a ProfileObjectType of a Zendesk user and “FirstName” is a
-- field in that ObjectType.
--
-- 'contentType', 'objectTypeField_contentType' - The content type of the field. Used for determining equality when
-- searching.
--
-- 'target', 'objectTypeField_target' - The location of the data in the standard ProfileObject model. For
-- example: _profile.Address.PostalCode.
newObjectTypeField ::
  ObjectTypeField
newObjectTypeField =
  ObjectTypeField'
    { source = Prelude.Nothing,
      contentType = Prelude.Nothing,
      target = Prelude.Nothing
    }

-- | A field of a ProfileObject. For example: _source.FirstName, where
-- “_source” is a ProfileObjectType of a Zendesk user and “FirstName” is a
-- field in that ObjectType.
objectTypeField_source :: Lens.Lens' ObjectTypeField (Prelude.Maybe Prelude.Text)
objectTypeField_source = Lens.lens (\ObjectTypeField' {source} -> source) (\s@ObjectTypeField' {} a -> s {source = a} :: ObjectTypeField)

-- | The content type of the field. Used for determining equality when
-- searching.
objectTypeField_contentType :: Lens.Lens' ObjectTypeField (Prelude.Maybe FieldContentType)
objectTypeField_contentType = Lens.lens (\ObjectTypeField' {contentType} -> contentType) (\s@ObjectTypeField' {} a -> s {contentType = a} :: ObjectTypeField)

-- | The location of the data in the standard ProfileObject model. For
-- example: _profile.Address.PostalCode.
objectTypeField_target :: Lens.Lens' ObjectTypeField (Prelude.Maybe Prelude.Text)
objectTypeField_target = Lens.lens (\ObjectTypeField' {target} -> target) (\s@ObjectTypeField' {} a -> s {target = a} :: ObjectTypeField)

instance Core.FromJSON ObjectTypeField where
  parseJSON =
    Core.withObject
      "ObjectTypeField"
      ( \x ->
          ObjectTypeField'
            Prelude.<$> (x Core..:? "Source")
            Prelude.<*> (x Core..:? "ContentType")
            Prelude.<*> (x Core..:? "Target")
      )

instance Prelude.Hashable ObjectTypeField

instance Prelude.NFData ObjectTypeField

instance Core.ToJSON ObjectTypeField where
  toJSON ObjectTypeField' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Source" Core..=) Prelude.<$> source,
            ("ContentType" Core..=) Prelude.<$> contentType,
            ("Target" Core..=) Prelude.<$> target
          ]
      )
