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
-- Module      : Amazonka.EC2.Types.AlternatePathHint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AlternatePathHint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes an potential intermediate component of a feasible path.
--
-- /See:/ 'newAlternatePathHint' smart constructor.
data AlternatePathHint = AlternatePathHint'
  { -- | The Amazon Resource Name (ARN) of the component.
    componentArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the component.
    componentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlternatePathHint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentArn', 'alternatePathHint_componentArn' - The Amazon Resource Name (ARN) of the component.
--
-- 'componentId', 'alternatePathHint_componentId' - The ID of the component.
newAlternatePathHint ::
  AlternatePathHint
newAlternatePathHint =
  AlternatePathHint'
    { componentArn = Prelude.Nothing,
      componentId = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the component.
alternatePathHint_componentArn :: Lens.Lens' AlternatePathHint (Prelude.Maybe Prelude.Text)
alternatePathHint_componentArn = Lens.lens (\AlternatePathHint' {componentArn} -> componentArn) (\s@AlternatePathHint' {} a -> s {componentArn = a} :: AlternatePathHint)

-- | The ID of the component.
alternatePathHint_componentId :: Lens.Lens' AlternatePathHint (Prelude.Maybe Prelude.Text)
alternatePathHint_componentId = Lens.lens (\AlternatePathHint' {componentId} -> componentId) (\s@AlternatePathHint' {} a -> s {componentId = a} :: AlternatePathHint)

instance Data.FromXML AlternatePathHint where
  parseXML x =
    AlternatePathHint'
      Prelude.<$> (x Data..@? "componentArn")
      Prelude.<*> (x Data..@? "componentId")

instance Prelude.Hashable AlternatePathHint where
  hashWithSalt _salt AlternatePathHint' {..} =
    _salt
      `Prelude.hashWithSalt` componentArn
      `Prelude.hashWithSalt` componentId

instance Prelude.NFData AlternatePathHint where
  rnf AlternatePathHint' {..} =
    Prelude.rnf componentArn
      `Prelude.seq` Prelude.rnf componentId
