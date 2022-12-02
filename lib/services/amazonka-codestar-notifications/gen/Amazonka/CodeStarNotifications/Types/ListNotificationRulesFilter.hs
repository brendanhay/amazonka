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
-- Module      : Amazonka.CodeStarNotifications.Types.ListNotificationRulesFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStarNotifications.Types.ListNotificationRulesFilter where

import Amazonka.CodeStarNotifications.Types.ListNotificationRulesFilterName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a filter to apply to the list of returned notification
-- rules. You can filter by event type, owner, resource, or target.
--
-- /See:/ 'newListNotificationRulesFilter' smart constructor.
data ListNotificationRulesFilter = ListNotificationRulesFilter'
  { -- | The name of the attribute you want to use to filter the returned
    -- notification rules.
    name :: ListNotificationRulesFilterName,
    -- | The value of the attribute you want to use to filter the returned
    -- notification rules. For example, if you specify filtering by /RESOURCE/
    -- in Name, you might specify the ARN of a pipeline in CodePipeline for the
    -- value.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNotificationRulesFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'listNotificationRulesFilter_name' - The name of the attribute you want to use to filter the returned
-- notification rules.
--
-- 'value', 'listNotificationRulesFilter_value' - The value of the attribute you want to use to filter the returned
-- notification rules. For example, if you specify filtering by /RESOURCE/
-- in Name, you might specify the ARN of a pipeline in CodePipeline for the
-- value.
newListNotificationRulesFilter ::
  -- | 'name'
  ListNotificationRulesFilterName ->
  -- | 'value'
  Prelude.Text ->
  ListNotificationRulesFilter
newListNotificationRulesFilter pName_ pValue_ =
  ListNotificationRulesFilter'
    { name = pName_,
      value = pValue_
    }

-- | The name of the attribute you want to use to filter the returned
-- notification rules.
listNotificationRulesFilter_name :: Lens.Lens' ListNotificationRulesFilter ListNotificationRulesFilterName
listNotificationRulesFilter_name = Lens.lens (\ListNotificationRulesFilter' {name} -> name) (\s@ListNotificationRulesFilter' {} a -> s {name = a} :: ListNotificationRulesFilter)

-- | The value of the attribute you want to use to filter the returned
-- notification rules. For example, if you specify filtering by /RESOURCE/
-- in Name, you might specify the ARN of a pipeline in CodePipeline for the
-- value.
listNotificationRulesFilter_value :: Lens.Lens' ListNotificationRulesFilter Prelude.Text
listNotificationRulesFilter_value = Lens.lens (\ListNotificationRulesFilter' {value} -> value) (\s@ListNotificationRulesFilter' {} a -> s {value = a} :: ListNotificationRulesFilter)

instance Prelude.Hashable ListNotificationRulesFilter where
  hashWithSalt _salt ListNotificationRulesFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData ListNotificationRulesFilter where
  rnf ListNotificationRulesFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON ListNotificationRulesFilter where
  toJSON ListNotificationRulesFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Value" Data..= value)
          ]
      )
