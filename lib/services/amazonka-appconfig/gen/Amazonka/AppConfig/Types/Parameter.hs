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
-- Module      : Amazonka.AppConfig.Types.Parameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.Parameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A value such as an Amazon Resource Name (ARN) or an Amazon Simple
-- Notification Service topic entered in an extension when invoked.
-- Parameter values are specified in an extension association. For more
-- information about extensions, see
-- <https://docs.aws.amazon.com/appconfig/latest/userguide/working-with-appconfig-extensions.html Working with AppConfig extensions>
-- in the /AppConfig User Guide/.
--
-- /See:/ 'newParameter' smart constructor.
data Parameter = Parameter'
  { -- | Information about the parameter.
    description :: Prelude.Maybe Prelude.Text,
    -- | A parameter value must be specified in the extension association.
    required :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Parameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'parameter_description' - Information about the parameter.
--
-- 'required', 'parameter_required' - A parameter value must be specified in the extension association.
newParameter ::
  Parameter
newParameter =
  Parameter'
    { description = Prelude.Nothing,
      required = Prelude.Nothing
    }

-- | Information about the parameter.
parameter_description :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_description = Lens.lens (\Parameter' {description} -> description) (\s@Parameter' {} a -> s {description = a} :: Parameter)

-- | A parameter value must be specified in the extension association.
parameter_required :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Bool)
parameter_required = Lens.lens (\Parameter' {required} -> required) (\s@Parameter' {} a -> s {required = a} :: Parameter)

instance Data.FromJSON Parameter where
  parseJSON =
    Data.withObject
      "Parameter"
      ( \x ->
          Parameter'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Required")
      )

instance Prelude.Hashable Parameter where
  hashWithSalt _salt Parameter' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` required

instance Prelude.NFData Parameter where
  rnf Parameter' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf required

instance Data.ToJSON Parameter where
  toJSON Parameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Required" Data..=) Prelude.<$> required
          ]
      )
