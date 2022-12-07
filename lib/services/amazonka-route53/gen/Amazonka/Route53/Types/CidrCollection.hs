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
-- Module      : Amazonka.Route53.Types.CidrCollection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.CidrCollection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal

-- | A complex type that identifies a CIDR collection.
--
-- /See:/ 'newCidrCollection' smart constructor.
data CidrCollection = CidrCollection'
  { -- | The name of a CIDR collection.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the collection. Can be used to reference the collection in
    -- IAM policy or in another Amazon Web Services account.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the CIDR collection.
    id :: Prelude.Maybe Prelude.Text,
    -- | A sequential counter that Route 53 sets to 1 when you create a CIDR
    -- collection and increments by 1 each time you update settings for the
    -- CIDR collection.
    version :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CidrCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'cidrCollection_name' - The name of a CIDR collection.
--
-- 'arn', 'cidrCollection_arn' - The ARN of the collection. Can be used to reference the collection in
-- IAM policy or in another Amazon Web Services account.
--
-- 'id', 'cidrCollection_id' - The unique ID of the CIDR collection.
--
-- 'version', 'cidrCollection_version' - A sequential counter that Route 53 sets to 1 when you create a CIDR
-- collection and increments by 1 each time you update settings for the
-- CIDR collection.
newCidrCollection ::
  CidrCollection
newCidrCollection =
  CidrCollection'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The name of a CIDR collection.
cidrCollection_name :: Lens.Lens' CidrCollection (Prelude.Maybe Prelude.Text)
cidrCollection_name = Lens.lens (\CidrCollection' {name} -> name) (\s@CidrCollection' {} a -> s {name = a} :: CidrCollection)

-- | The ARN of the collection. Can be used to reference the collection in
-- IAM policy or in another Amazon Web Services account.
cidrCollection_arn :: Lens.Lens' CidrCollection (Prelude.Maybe Prelude.Text)
cidrCollection_arn = Lens.lens (\CidrCollection' {arn} -> arn) (\s@CidrCollection' {} a -> s {arn = a} :: CidrCollection)

-- | The unique ID of the CIDR collection.
cidrCollection_id :: Lens.Lens' CidrCollection (Prelude.Maybe Prelude.Text)
cidrCollection_id = Lens.lens (\CidrCollection' {id} -> id) (\s@CidrCollection' {} a -> s {id = a} :: CidrCollection)

-- | A sequential counter that Route 53 sets to 1 when you create a CIDR
-- collection and increments by 1 each time you update settings for the
-- CIDR collection.
cidrCollection_version :: Lens.Lens' CidrCollection (Prelude.Maybe Prelude.Natural)
cidrCollection_version = Lens.lens (\CidrCollection' {version} -> version) (\s@CidrCollection' {} a -> s {version = a} :: CidrCollection)

instance Data.FromXML CidrCollection where
  parseXML x =
    CidrCollection'
      Prelude.<$> (x Data..@? "Name")
      Prelude.<*> (x Data..@? "Arn")
      Prelude.<*> (x Data..@? "Id")
      Prelude.<*> (x Data..@? "Version")

instance Prelude.Hashable CidrCollection where
  hashWithSalt _salt CidrCollection' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` version

instance Prelude.NFData CidrCollection where
  rnf CidrCollection' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf version
