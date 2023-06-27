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
-- Module      : Amazonka.S3.Types.ObjectIdentifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.ObjectIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

-- | Object Identifier is unique value to identify objects.
--
-- /See:/ 'newObjectIdentifier' smart constructor.
data ObjectIdentifier = ObjectIdentifier'
  { -- | VersionId for the specific version of the object to delete.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | Key name of the object.
    --
    -- Replacement must be made for object keys containing special characters
    -- (such as carriage returns) when using XML requests. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
    key :: ObjectKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ObjectIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'objectIdentifier_versionId' - VersionId for the specific version of the object to delete.
--
-- 'key', 'objectIdentifier_key' - Key name of the object.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
newObjectIdentifier ::
  -- | 'key'
  ObjectKey ->
  ObjectIdentifier
newObjectIdentifier pKey_ =
  ObjectIdentifier'
    { versionId = Prelude.Nothing,
      key = pKey_
    }

-- | VersionId for the specific version of the object to delete.
objectIdentifier_versionId :: Lens.Lens' ObjectIdentifier (Prelude.Maybe ObjectVersionId)
objectIdentifier_versionId = Lens.lens (\ObjectIdentifier' {versionId} -> versionId) (\s@ObjectIdentifier' {} a -> s {versionId = a} :: ObjectIdentifier)

-- | Key name of the object.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
objectIdentifier_key :: Lens.Lens' ObjectIdentifier ObjectKey
objectIdentifier_key = Lens.lens (\ObjectIdentifier' {key} -> key) (\s@ObjectIdentifier' {} a -> s {key = a} :: ObjectIdentifier)

instance Prelude.Hashable ObjectIdentifier where
  hashWithSalt _salt ObjectIdentifier' {..} =
    _salt
      `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` key

instance Prelude.NFData ObjectIdentifier where
  rnf ObjectIdentifier' {..} =
    Prelude.rnf versionId `Prelude.seq` Prelude.rnf key

instance Data.ToXML ObjectIdentifier where
  toXML ObjectIdentifier' {..} =
    Prelude.mconcat
      ["VersionId" Data.@= versionId, "Key" Data.@= key]
