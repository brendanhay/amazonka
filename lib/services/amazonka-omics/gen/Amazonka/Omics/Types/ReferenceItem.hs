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
-- Module      : Amazonka.Omics.Types.ReferenceItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ReferenceItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A genome reference.
--
-- /See:/ 'newReferenceItem' smart constructor.
data ReferenceItem = ReferenceItem'
  { -- | The reference\'s ARN.
    referenceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReferenceItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'referenceArn', 'referenceItem_referenceArn' - The reference\'s ARN.
newReferenceItem ::
  ReferenceItem
newReferenceItem =
  ReferenceItem' {referenceArn = Prelude.Nothing}

-- | The reference\'s ARN.
referenceItem_referenceArn :: Lens.Lens' ReferenceItem (Prelude.Maybe Prelude.Text)
referenceItem_referenceArn = Lens.lens (\ReferenceItem' {referenceArn} -> referenceArn) (\s@ReferenceItem' {} a -> s {referenceArn = a} :: ReferenceItem)

instance Data.FromJSON ReferenceItem where
  parseJSON =
    Data.withObject
      "ReferenceItem"
      ( \x ->
          ReferenceItem'
            Prelude.<$> (x Data..:? "referenceArn")
      )

instance Prelude.Hashable ReferenceItem where
  hashWithSalt _salt ReferenceItem' {..} =
    _salt `Prelude.hashWithSalt` referenceArn

instance Prelude.NFData ReferenceItem where
  rnf ReferenceItem' {..} = Prelude.rnf referenceArn

instance Data.ToJSON ReferenceItem where
  toJSON ReferenceItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [("referenceArn" Data..=) Prelude.<$> referenceArn]
      )
