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
-- Module      : Amazonka.EC2.Types.IamInstanceProfileSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IamInstanceProfileSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes an IAM instance profile.
--
-- /See:/ 'newIamInstanceProfileSpecification' smart constructor.
data IamInstanceProfileSpecification = IamInstanceProfileSpecification'
  { -- | The Amazon Resource Name (ARN) of the instance profile.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the instance profile.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IamInstanceProfileSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'iamInstanceProfileSpecification_arn' - The Amazon Resource Name (ARN) of the instance profile.
--
-- 'name', 'iamInstanceProfileSpecification_name' - The name of the instance profile.
newIamInstanceProfileSpecification ::
  IamInstanceProfileSpecification
newIamInstanceProfileSpecification =
  IamInstanceProfileSpecification'
    { arn =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance profile.
iamInstanceProfileSpecification_arn :: Lens.Lens' IamInstanceProfileSpecification (Prelude.Maybe Prelude.Text)
iamInstanceProfileSpecification_arn = Lens.lens (\IamInstanceProfileSpecification' {arn} -> arn) (\s@IamInstanceProfileSpecification' {} a -> s {arn = a} :: IamInstanceProfileSpecification)

-- | The name of the instance profile.
iamInstanceProfileSpecification_name :: Lens.Lens' IamInstanceProfileSpecification (Prelude.Maybe Prelude.Text)
iamInstanceProfileSpecification_name = Lens.lens (\IamInstanceProfileSpecification' {name} -> name) (\s@IamInstanceProfileSpecification' {} a -> s {name = a} :: IamInstanceProfileSpecification)

instance Data.FromXML IamInstanceProfileSpecification where
  parseXML x =
    IamInstanceProfileSpecification'
      Prelude.<$> (x Data..@? "arn")
      Prelude.<*> (x Data..@? "name")

instance
  Prelude.Hashable
    IamInstanceProfileSpecification
  where
  hashWithSalt
    _salt
    IamInstanceProfileSpecification' {..} =
      _salt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    IamInstanceProfileSpecification
  where
  rnf IamInstanceProfileSpecification' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf name

instance Data.ToQuery IamInstanceProfileSpecification where
  toQuery IamInstanceProfileSpecification' {..} =
    Prelude.mconcat
      ["Arn" Data.=: arn, "Name" Data.=: name]
