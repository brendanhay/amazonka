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
-- Module      : Amazonka.AmplifyUiBuilder.Types.PutMetadataFlagBody
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.PutMetadataFlagBody where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Stores the metadata information about a feature on a form or view.
--
-- /See:/ 'newPutMetadataFlagBody' smart constructor.
data PutMetadataFlagBody = PutMetadataFlagBody'
  { -- | The new information to store.
    newValue' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutMetadataFlagBody' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'newValue'', 'putMetadataFlagBody_newValue' - The new information to store.
newPutMetadataFlagBody ::
  -- | 'newValue''
  Prelude.Text ->
  PutMetadataFlagBody
newPutMetadataFlagBody pNewValue_ =
  PutMetadataFlagBody' {newValue' = pNewValue_}

-- | The new information to store.
putMetadataFlagBody_newValue :: Lens.Lens' PutMetadataFlagBody Prelude.Text
putMetadataFlagBody_newValue = Lens.lens (\PutMetadataFlagBody' {newValue'} -> newValue') (\s@PutMetadataFlagBody' {} a -> s {newValue' = a} :: PutMetadataFlagBody)

instance Prelude.Hashable PutMetadataFlagBody where
  hashWithSalt _salt PutMetadataFlagBody' {..} =
    _salt `Prelude.hashWithSalt` newValue'

instance Prelude.NFData PutMetadataFlagBody where
  rnf PutMetadataFlagBody' {..} = Prelude.rnf newValue'

instance Data.ToJSON PutMetadataFlagBody where
  toJSON PutMetadataFlagBody' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("newValue" Data..= newValue')]
      )
