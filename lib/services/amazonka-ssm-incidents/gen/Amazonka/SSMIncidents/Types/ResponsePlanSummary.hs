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
-- Module      : Amazonka.SSMIncidents.Types.ResponsePlanSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.ResponsePlanSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details of the response plan that are used when creating an incident.
--
-- /See:/ 'newResponsePlanSummary' smart constructor.
data ResponsePlanSummary = ResponsePlanSummary'
  { -- | The human readable name of the response plan. This can include spaces.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the response plan.
    arn :: Prelude.Text,
    -- | The name of the response plan. This can\'t include spaces.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponsePlanSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'responsePlanSummary_displayName' - The human readable name of the response plan. This can include spaces.
--
-- 'arn', 'responsePlanSummary_arn' - The Amazon Resource Name (ARN) of the response plan.
--
-- 'name', 'responsePlanSummary_name' - The name of the response plan. This can\'t include spaces.
newResponsePlanSummary ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  ResponsePlanSummary
newResponsePlanSummary pArn_ pName_ =
  ResponsePlanSummary'
    { displayName = Prelude.Nothing,
      arn = pArn_,
      name = pName_
    }

-- | The human readable name of the response plan. This can include spaces.
responsePlanSummary_displayName :: Lens.Lens' ResponsePlanSummary (Prelude.Maybe Prelude.Text)
responsePlanSummary_displayName = Lens.lens (\ResponsePlanSummary' {displayName} -> displayName) (\s@ResponsePlanSummary' {} a -> s {displayName = a} :: ResponsePlanSummary)

-- | The Amazon Resource Name (ARN) of the response plan.
responsePlanSummary_arn :: Lens.Lens' ResponsePlanSummary Prelude.Text
responsePlanSummary_arn = Lens.lens (\ResponsePlanSummary' {arn} -> arn) (\s@ResponsePlanSummary' {} a -> s {arn = a} :: ResponsePlanSummary)

-- | The name of the response plan. This can\'t include spaces.
responsePlanSummary_name :: Lens.Lens' ResponsePlanSummary Prelude.Text
responsePlanSummary_name = Lens.lens (\ResponsePlanSummary' {name} -> name) (\s@ResponsePlanSummary' {} a -> s {name = a} :: ResponsePlanSummary)

instance Data.FromJSON ResponsePlanSummary where
  parseJSON =
    Data.withObject
      "ResponsePlanSummary"
      ( \x ->
          ResponsePlanSummary'
            Prelude.<$> (x Data..:? "displayName")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable ResponsePlanSummary where
  hashWithSalt _salt ResponsePlanSummary' {..} =
    _salt `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name

instance Prelude.NFData ResponsePlanSummary where
  rnf ResponsePlanSummary' {..} =
    Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
