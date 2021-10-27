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
-- Module      : Network.AWS.Kendra.Types.DocumentAttributeValueCountPair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.DocumentAttributeValueCountPair where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.DocumentAttributeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the count of documents that match a particular attribute when
-- doing a faceted search.
--
-- /See:/ 'newDocumentAttributeValueCountPair' smart constructor.
data DocumentAttributeValueCountPair = DocumentAttributeValueCountPair'
  { -- | The number of documents in the response that have the attribute value
    -- for the key.
    count :: Prelude.Maybe Prelude.Int,
    -- | The value of the attribute. For example, \"HR.\"
    documentAttributeValue :: Prelude.Maybe DocumentAttributeValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentAttributeValueCountPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'documentAttributeValueCountPair_count' - The number of documents in the response that have the attribute value
-- for the key.
--
-- 'documentAttributeValue', 'documentAttributeValueCountPair_documentAttributeValue' - The value of the attribute. For example, \"HR.\"
newDocumentAttributeValueCountPair ::
  DocumentAttributeValueCountPair
newDocumentAttributeValueCountPair =
  DocumentAttributeValueCountPair'
    { count =
        Prelude.Nothing,
      documentAttributeValue = Prelude.Nothing
    }

-- | The number of documents in the response that have the attribute value
-- for the key.
documentAttributeValueCountPair_count :: Lens.Lens' DocumentAttributeValueCountPair (Prelude.Maybe Prelude.Int)
documentAttributeValueCountPair_count = Lens.lens (\DocumentAttributeValueCountPair' {count} -> count) (\s@DocumentAttributeValueCountPair' {} a -> s {count = a} :: DocumentAttributeValueCountPair)

-- | The value of the attribute. For example, \"HR.\"
documentAttributeValueCountPair_documentAttributeValue :: Lens.Lens' DocumentAttributeValueCountPair (Prelude.Maybe DocumentAttributeValue)
documentAttributeValueCountPair_documentAttributeValue = Lens.lens (\DocumentAttributeValueCountPair' {documentAttributeValue} -> documentAttributeValue) (\s@DocumentAttributeValueCountPair' {} a -> s {documentAttributeValue = a} :: DocumentAttributeValueCountPair)

instance
  Core.FromJSON
    DocumentAttributeValueCountPair
  where
  parseJSON =
    Core.withObject
      "DocumentAttributeValueCountPair"
      ( \x ->
          DocumentAttributeValueCountPair'
            Prelude.<$> (x Core..:? "Count")
            Prelude.<*> (x Core..:? "DocumentAttributeValue")
      )

instance
  Prelude.Hashable
    DocumentAttributeValueCountPair

instance
  Prelude.NFData
    DocumentAttributeValueCountPair
