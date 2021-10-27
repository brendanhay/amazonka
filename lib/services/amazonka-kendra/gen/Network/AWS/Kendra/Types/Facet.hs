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
-- Module      : Network.AWS.Kendra.Types.Facet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.Facet where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a document attribute
--
-- /See:/ 'newFacet' smart constructor.
data Facet = Facet'
  { -- | The unique key for the document attribute.
    documentAttributeKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Facet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentAttributeKey', 'facet_documentAttributeKey' - The unique key for the document attribute.
newFacet ::
  Facet
newFacet =
  Facet' {documentAttributeKey = Prelude.Nothing}

-- | The unique key for the document attribute.
facet_documentAttributeKey :: Lens.Lens' Facet (Prelude.Maybe Prelude.Text)
facet_documentAttributeKey = Lens.lens (\Facet' {documentAttributeKey} -> documentAttributeKey) (\s@Facet' {} a -> s {documentAttributeKey = a} :: Facet)

instance Prelude.Hashable Facet

instance Prelude.NFData Facet

instance Core.ToJSON Facet where
  toJSON Facet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DocumentAttributeKey" Core..=)
              Prelude.<$> documentAttributeKey
          ]
      )
