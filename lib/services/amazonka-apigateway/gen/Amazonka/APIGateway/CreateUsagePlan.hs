{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.APIGateway.CreateUsagePlan
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a usage plan with the throttle and quota limits, as well as the
-- associated API stages, specified in the payload.
module Amazonka.APIGateway.CreateUsagePlan
  ( -- * Creating a Request
    CreateUsagePlan (..),
    newCreateUsagePlan,

    -- * Request Lenses
    createUsagePlan_apiStages,
    createUsagePlan_throttle,
    createUsagePlan_quota,
    createUsagePlan_description,
    createUsagePlan_tags,
    createUsagePlan_name,

    -- * Destructuring the Response
    UsagePlan (..),
    newUsagePlan,

    -- * Response Lenses
    usagePlan_apiStages,
    usagePlan_name,
    usagePlan_id,
    usagePlan_throttle,
    usagePlan_quota,
    usagePlan_description,
    usagePlan_productCode,
    usagePlan_tags,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The POST request to create a usage plan with the name, description,
-- throttle limits and quota limits, as well as the associated API stages,
-- specified in the payload.
--
-- /See:/ 'newCreateUsagePlan' smart constructor.
data CreateUsagePlan = CreateUsagePlan'
  { -- | The associated API stages of the usage plan.
    apiStages :: Prelude.Maybe [ApiStage],
    -- | The throttling limits of the usage plan.
    throttle :: Prelude.Maybe ThrottleSettings,
    -- | The quota of the usage plan.
    quota :: Prelude.Maybe QuotaSettings,
    -- | The description of the usage plan.
    description :: Prelude.Maybe Prelude.Text,
    -- | The key-value map of strings. The valid character set is
    -- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
    -- start with @aws:@. The tag value can be up to 256 characters.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | [Required] The name of the usage plan.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUsagePlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiStages', 'createUsagePlan_apiStages' - The associated API stages of the usage plan.
--
-- 'throttle', 'createUsagePlan_throttle' - The throttling limits of the usage plan.
--
-- 'quota', 'createUsagePlan_quota' - The quota of the usage plan.
--
-- 'description', 'createUsagePlan_description' - The description of the usage plan.
--
-- 'tags', 'createUsagePlan_tags' - The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
--
-- 'name', 'createUsagePlan_name' - [Required] The name of the usage plan.
newCreateUsagePlan ::
  -- | 'name'
  Prelude.Text ->
  CreateUsagePlan
newCreateUsagePlan pName_ =
  CreateUsagePlan'
    { apiStages = Prelude.Nothing,
      throttle = Prelude.Nothing,
      quota = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | The associated API stages of the usage plan.
createUsagePlan_apiStages :: Lens.Lens' CreateUsagePlan (Prelude.Maybe [ApiStage])
createUsagePlan_apiStages = Lens.lens (\CreateUsagePlan' {apiStages} -> apiStages) (\s@CreateUsagePlan' {} a -> s {apiStages = a} :: CreateUsagePlan) Prelude.. Lens.mapping Lens.coerced

-- | The throttling limits of the usage plan.
createUsagePlan_throttle :: Lens.Lens' CreateUsagePlan (Prelude.Maybe ThrottleSettings)
createUsagePlan_throttle = Lens.lens (\CreateUsagePlan' {throttle} -> throttle) (\s@CreateUsagePlan' {} a -> s {throttle = a} :: CreateUsagePlan)

-- | The quota of the usage plan.
createUsagePlan_quota :: Lens.Lens' CreateUsagePlan (Prelude.Maybe QuotaSettings)
createUsagePlan_quota = Lens.lens (\CreateUsagePlan' {quota} -> quota) (\s@CreateUsagePlan' {} a -> s {quota = a} :: CreateUsagePlan)

-- | The description of the usage plan.
createUsagePlan_description :: Lens.Lens' CreateUsagePlan (Prelude.Maybe Prelude.Text)
createUsagePlan_description = Lens.lens (\CreateUsagePlan' {description} -> description) (\s@CreateUsagePlan' {} a -> s {description = a} :: CreateUsagePlan)

-- | The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
createUsagePlan_tags :: Lens.Lens' CreateUsagePlan (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createUsagePlan_tags = Lens.lens (\CreateUsagePlan' {tags} -> tags) (\s@CreateUsagePlan' {} a -> s {tags = a} :: CreateUsagePlan) Prelude.. Lens.mapping Lens.coerced

-- | [Required] The name of the usage plan.
createUsagePlan_name :: Lens.Lens' CreateUsagePlan Prelude.Text
createUsagePlan_name = Lens.lens (\CreateUsagePlan' {name} -> name) (\s@CreateUsagePlan' {} a -> s {name = a} :: CreateUsagePlan)

instance Core.AWSRequest CreateUsagePlan where
  type AWSResponse CreateUsagePlan = UsagePlan
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable CreateUsagePlan

instance Prelude.NFData CreateUsagePlan

instance Core.ToHeaders CreateUsagePlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToJSON CreateUsagePlan where
  toJSON CreateUsagePlan' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("apiStages" Core..=) Prelude.<$> apiStages,
            ("throttle" Core..=) Prelude.<$> throttle,
            ("quota" Core..=) Prelude.<$> quota,
            ("description" Core..=) Prelude.<$> description,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath CreateUsagePlan where
  toPath = Prelude.const "/usageplans"

instance Core.ToQuery CreateUsagePlan where
  toQuery = Prelude.const Prelude.mempty
