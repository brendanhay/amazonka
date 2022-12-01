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
-- Module      : Amazonka.CodeDeploy.Types.OnPremisesTagSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.OnPremisesTagSet where

import Amazonka.CodeDeploy.Types.TagFilter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about groups of on-premises instance tags.
--
-- /See:/ 'newOnPremisesTagSet' smart constructor.
data OnPremisesTagSet = OnPremisesTagSet'
  { -- | A list that contains other lists of on-premises instance tag groups. For
    -- an instance to be included in the deployment group, it must be
    -- identified by all of the tag groups in the list.
    onPremisesTagSetList :: Prelude.Maybe [[TagFilter]]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OnPremisesTagSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'onPremisesTagSetList', 'onPremisesTagSet_onPremisesTagSetList' - A list that contains other lists of on-premises instance tag groups. For
-- an instance to be included in the deployment group, it must be
-- identified by all of the tag groups in the list.
newOnPremisesTagSet ::
  OnPremisesTagSet
newOnPremisesTagSet =
  OnPremisesTagSet'
    { onPremisesTagSetList =
        Prelude.Nothing
    }

-- | A list that contains other lists of on-premises instance tag groups. For
-- an instance to be included in the deployment group, it must be
-- identified by all of the tag groups in the list.
onPremisesTagSet_onPremisesTagSetList :: Lens.Lens' OnPremisesTagSet (Prelude.Maybe [[TagFilter]])
onPremisesTagSet_onPremisesTagSetList = Lens.lens (\OnPremisesTagSet' {onPremisesTagSetList} -> onPremisesTagSetList) (\s@OnPremisesTagSet' {} a -> s {onPremisesTagSetList = a} :: OnPremisesTagSet) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON OnPremisesTagSet where
  parseJSON =
    Core.withObject
      "OnPremisesTagSet"
      ( \x ->
          OnPremisesTagSet'
            Prelude.<$> ( x Core..:? "onPremisesTagSetList"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable OnPremisesTagSet where
  hashWithSalt _salt OnPremisesTagSet' {..} =
    _salt `Prelude.hashWithSalt` onPremisesTagSetList

instance Prelude.NFData OnPremisesTagSet where
  rnf OnPremisesTagSet' {..} =
    Prelude.rnf onPremisesTagSetList

instance Core.ToJSON OnPremisesTagSet where
  toJSON OnPremisesTagSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("onPremisesTagSetList" Core..=)
              Prelude.<$> onPremisesTagSetList
          ]
      )
