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
-- Module      : Amazonka.FraudDetector.Types.AllowDenyList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.AllowDenyList where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The metadata of a list.
--
-- /See:/ 'newAllowDenyList' smart constructor.
data AllowDenyList = AllowDenyList'
  { -- | The ARN of the list.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time the list was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The description of the list.
    description :: Prelude.Maybe Prelude.Text,
    -- | The time the list was last updated.
    updatedTime :: Prelude.Maybe Prelude.Text,
    -- | The variable type of the list.
    variableType :: Prelude.Maybe Prelude.Text,
    -- | The name of the list.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AllowDenyList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'allowDenyList_arn' - The ARN of the list.
--
-- 'createdTime', 'allowDenyList_createdTime' - The time the list was created.
--
-- 'description', 'allowDenyList_description' - The description of the list.
--
-- 'updatedTime', 'allowDenyList_updatedTime' - The time the list was last updated.
--
-- 'variableType', 'allowDenyList_variableType' - The variable type of the list.
--
-- 'name', 'allowDenyList_name' - The name of the list.
newAllowDenyList ::
  -- | 'name'
  Prelude.Text ->
  AllowDenyList
newAllowDenyList pName_ =
  AllowDenyList'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      description = Prelude.Nothing,
      updatedTime = Prelude.Nothing,
      variableType = Prelude.Nothing,
      name = pName_
    }

-- | The ARN of the list.
allowDenyList_arn :: Lens.Lens' AllowDenyList (Prelude.Maybe Prelude.Text)
allowDenyList_arn = Lens.lens (\AllowDenyList' {arn} -> arn) (\s@AllowDenyList' {} a -> s {arn = a} :: AllowDenyList)

-- | The time the list was created.
allowDenyList_createdTime :: Lens.Lens' AllowDenyList (Prelude.Maybe Prelude.Text)
allowDenyList_createdTime = Lens.lens (\AllowDenyList' {createdTime} -> createdTime) (\s@AllowDenyList' {} a -> s {createdTime = a} :: AllowDenyList)

-- | The description of the list.
allowDenyList_description :: Lens.Lens' AllowDenyList (Prelude.Maybe Prelude.Text)
allowDenyList_description = Lens.lens (\AllowDenyList' {description} -> description) (\s@AllowDenyList' {} a -> s {description = a} :: AllowDenyList)

-- | The time the list was last updated.
allowDenyList_updatedTime :: Lens.Lens' AllowDenyList (Prelude.Maybe Prelude.Text)
allowDenyList_updatedTime = Lens.lens (\AllowDenyList' {updatedTime} -> updatedTime) (\s@AllowDenyList' {} a -> s {updatedTime = a} :: AllowDenyList)

-- | The variable type of the list.
allowDenyList_variableType :: Lens.Lens' AllowDenyList (Prelude.Maybe Prelude.Text)
allowDenyList_variableType = Lens.lens (\AllowDenyList' {variableType} -> variableType) (\s@AllowDenyList' {} a -> s {variableType = a} :: AllowDenyList)

-- | The name of the list.
allowDenyList_name :: Lens.Lens' AllowDenyList Prelude.Text
allowDenyList_name = Lens.lens (\AllowDenyList' {name} -> name) (\s@AllowDenyList' {} a -> s {name = a} :: AllowDenyList)

instance Data.FromJSON AllowDenyList where
  parseJSON =
    Data.withObject
      "AllowDenyList"
      ( \x ->
          AllowDenyList'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdTime")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "updatedTime")
            Prelude.<*> (x Data..:? "variableType")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable AllowDenyList where
  hashWithSalt _salt AllowDenyList' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` updatedTime
      `Prelude.hashWithSalt` variableType
      `Prelude.hashWithSalt` name

instance Prelude.NFData AllowDenyList where
  rnf AllowDenyList' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf updatedTime
      `Prelude.seq` Prelude.rnf variableType
      `Prelude.seq` Prelude.rnf name
