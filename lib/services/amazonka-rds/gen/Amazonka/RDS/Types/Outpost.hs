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
-- Module      : Amazonka.RDS.Types.Outpost
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.Outpost where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A data type that represents an Outpost.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on Amazon Web Services Outposts>
-- in the /Amazon RDS User Guide./
--
-- /See:/ 'newOutpost' smart constructor.
data Outpost = Outpost'
  { -- | The Amazon Resource Name (ARN) of the Outpost.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Outpost' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'outpost_arn' - The Amazon Resource Name (ARN) of the Outpost.
newOutpost ::
  Outpost
newOutpost = Outpost' {arn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of the Outpost.
outpost_arn :: Lens.Lens' Outpost (Prelude.Maybe Prelude.Text)
outpost_arn = Lens.lens (\Outpost' {arn} -> arn) (\s@Outpost' {} a -> s {arn = a} :: Outpost)

instance Core.FromXML Outpost where
  parseXML x = Outpost' Prelude.<$> (x Core..@? "Arn")

instance Prelude.Hashable Outpost where
  hashWithSalt _salt Outpost' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData Outpost where
  rnf Outpost' {..} = Prelude.rnf arn
