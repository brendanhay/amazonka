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
-- Module      : Amazonka.LakeFormation.Types.VirtualObject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.VirtualObject where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that defines an Amazon S3 object to be deleted if a
-- transaction cancels, provided that @VirtualPut@ was called before
-- writing the object.
--
-- /See:/ 'newVirtualObject' smart constructor.
data VirtualObject = VirtualObject'
  { -- | The ETag of the Amazon S3 object.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The path to the Amazon S3 object. Must start with s3:\/\/
    uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'virtualObject_eTag' - The ETag of the Amazon S3 object.
--
-- 'uri', 'virtualObject_uri' - The path to the Amazon S3 object. Must start with s3:\/\/
newVirtualObject ::
  -- | 'uri'
  Prelude.Text ->
  VirtualObject
newVirtualObject pUri_ =
  VirtualObject' {eTag = Prelude.Nothing, uri = pUri_}

-- | The ETag of the Amazon S3 object.
virtualObject_eTag :: Lens.Lens' VirtualObject (Prelude.Maybe Prelude.Text)
virtualObject_eTag = Lens.lens (\VirtualObject' {eTag} -> eTag) (\s@VirtualObject' {} a -> s {eTag = a} :: VirtualObject)

-- | The path to the Amazon S3 object. Must start with s3:\/\/
virtualObject_uri :: Lens.Lens' VirtualObject Prelude.Text
virtualObject_uri = Lens.lens (\VirtualObject' {uri} -> uri) (\s@VirtualObject' {} a -> s {uri = a} :: VirtualObject)

instance Prelude.Hashable VirtualObject where
  hashWithSalt _salt VirtualObject' {..} =
    _salt `Prelude.hashWithSalt` eTag
      `Prelude.hashWithSalt` uri

instance Prelude.NFData VirtualObject where
  rnf VirtualObject' {..} =
    Prelude.rnf eTag `Prelude.seq` Prelude.rnf uri

instance Core.ToJSON VirtualObject where
  toJSON VirtualObject' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ETag" Core..=) Prelude.<$> eTag,
            Prelude.Just ("Uri" Core..= uri)
          ]
      )
