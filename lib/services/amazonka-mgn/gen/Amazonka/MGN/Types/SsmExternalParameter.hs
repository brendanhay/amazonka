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
-- Module      : Amazonka.MGN.Types.SsmExternalParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.SsmExternalParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | AWS Systems Manager Document external parameter.
--
-- /See:/ 'newSsmExternalParameter' smart constructor.
data SsmExternalParameter = SsmExternalParameter'
  { -- | AWS Systems Manager Document external parameters dynamic path.
    dynamicPath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SsmExternalParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dynamicPath', 'ssmExternalParameter_dynamicPath' - AWS Systems Manager Document external parameters dynamic path.
newSsmExternalParameter ::
  SsmExternalParameter
newSsmExternalParameter =
  SsmExternalParameter'
    { dynamicPath =
        Prelude.Nothing
    }

-- | AWS Systems Manager Document external parameters dynamic path.
ssmExternalParameter_dynamicPath :: Lens.Lens' SsmExternalParameter (Prelude.Maybe Prelude.Text)
ssmExternalParameter_dynamicPath = Lens.lens (\SsmExternalParameter' {dynamicPath} -> dynamicPath) (\s@SsmExternalParameter' {} a -> s {dynamicPath = a} :: SsmExternalParameter)

instance Data.FromJSON SsmExternalParameter where
  parseJSON =
    Data.withObject
      "SsmExternalParameter"
      ( \x ->
          SsmExternalParameter'
            Prelude.<$> (x Data..:? "dynamicPath")
      )

instance Prelude.Hashable SsmExternalParameter where
  hashWithSalt _salt SsmExternalParameter' {..} =
    _salt `Prelude.hashWithSalt` dynamicPath

instance Prelude.NFData SsmExternalParameter where
  rnf SsmExternalParameter' {..} =
    Prelude.rnf dynamicPath

instance Data.ToJSON SsmExternalParameter where
  toJSON SsmExternalParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [("dynamicPath" Data..=) Prelude.<$> dynamicPath]
      )
