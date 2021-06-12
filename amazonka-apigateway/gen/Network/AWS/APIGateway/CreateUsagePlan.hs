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
-- Module      : Network.AWS.APIGateway.CreateUsagePlan
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a usage plan with the throttle and quota limits, as well as the
-- associated API stages, specified in the payload.
module Network.AWS.APIGateway.CreateUsagePlan
  ( -- * Creating a Request
    CreateUsagePlan (..),
    newCreateUsagePlan,

    -- * Request Lenses
    createUsagePlan_apiStages,
    createUsagePlan_tags,
    createUsagePlan_description,
    createUsagePlan_quota,
    createUsagePlan_throttle,
    createUsagePlan_name,

    -- * Destructuring the Response
    UsagePlan (..),
    newUsagePlan,

    -- * Response Lenses
    usagePlan_id,
    usagePlan_name,
    usagePlan_apiStages,
    usagePlan_tags,
    usagePlan_description,
    usagePlan_quota,
    usagePlan_productCode,
    usagePlan_throttle,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The POST request to create a usage plan with the name, description,
-- throttle limits and quota limits, as well as the associated API stages,
-- specified in the payload.
--
-- /See:/ 'newCreateUsagePlan' smart constructor.
data CreateUsagePlan = CreateUsagePlan'
  { -- | The associated API stages of the usage plan.
    apiStages :: Core.Maybe [ApiStage],
    -- | The key-value map of strings. The valid character set is
    -- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
    -- start with @aws:@. The tag value can be up to 256 characters.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The description of the usage plan.
    description :: Core.Maybe Core.Text,
    -- | The quota of the usage plan.
    quota :: Core.Maybe QuotaSettings,
    -- | The throttling limits of the usage plan.
    throttle :: Core.Maybe ThrottleSettings,
    -- | [Required] The name of the usage plan.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'tags', 'createUsagePlan_tags' - The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
--
-- 'description', 'createUsagePlan_description' - The description of the usage plan.
--
-- 'quota', 'createUsagePlan_quota' - The quota of the usage plan.
--
-- 'throttle', 'createUsagePlan_throttle' - The throttling limits of the usage plan.
--
-- 'name', 'createUsagePlan_name' - [Required] The name of the usage plan.
newCreateUsagePlan ::
  -- | 'name'
  Core.Text ->
  CreateUsagePlan
newCreateUsagePlan pName_ =
  CreateUsagePlan'
    { apiStages = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      quota = Core.Nothing,
      throttle = Core.Nothing,
      name = pName_
    }

-- | The associated API stages of the usage plan.
createUsagePlan_apiStages :: Lens.Lens' CreateUsagePlan (Core.Maybe [ApiStage])
createUsagePlan_apiStages = Lens.lens (\CreateUsagePlan' {apiStages} -> apiStages) (\s@CreateUsagePlan' {} a -> s {apiStages = a} :: CreateUsagePlan) Core.. Lens.mapping Lens._Coerce

-- | The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
createUsagePlan_tags :: Lens.Lens' CreateUsagePlan (Core.Maybe (Core.HashMap Core.Text Core.Text))
createUsagePlan_tags = Lens.lens (\CreateUsagePlan' {tags} -> tags) (\s@CreateUsagePlan' {} a -> s {tags = a} :: CreateUsagePlan) Core.. Lens.mapping Lens._Coerce

-- | The description of the usage plan.
createUsagePlan_description :: Lens.Lens' CreateUsagePlan (Core.Maybe Core.Text)
createUsagePlan_description = Lens.lens (\CreateUsagePlan' {description} -> description) (\s@CreateUsagePlan' {} a -> s {description = a} :: CreateUsagePlan)

-- | The quota of the usage plan.
createUsagePlan_quota :: Lens.Lens' CreateUsagePlan (Core.Maybe QuotaSettings)
createUsagePlan_quota = Lens.lens (\CreateUsagePlan' {quota} -> quota) (\s@CreateUsagePlan' {} a -> s {quota = a} :: CreateUsagePlan)

-- | The throttling limits of the usage plan.
createUsagePlan_throttle :: Lens.Lens' CreateUsagePlan (Core.Maybe ThrottleSettings)
createUsagePlan_throttle = Lens.lens (\CreateUsagePlan' {throttle} -> throttle) (\s@CreateUsagePlan' {} a -> s {throttle = a} :: CreateUsagePlan)

-- | [Required] The name of the usage plan.
createUsagePlan_name :: Lens.Lens' CreateUsagePlan Core.Text
createUsagePlan_name = Lens.lens (\CreateUsagePlan' {name} -> name) (\s@CreateUsagePlan' {} a -> s {name = a} :: CreateUsagePlan)

instance Core.AWSRequest CreateUsagePlan where
  type AWSResponse CreateUsagePlan = UsagePlan
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable CreateUsagePlan

instance Core.NFData CreateUsagePlan

instance Core.ToHeaders CreateUsagePlan where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateUsagePlan where
  toJSON CreateUsagePlan' {..} =
    Core.object
      ( Core.catMaybes
          [ ("apiStages" Core..=) Core.<$> apiStages,
            ("tags" Core..=) Core.<$> tags,
            ("description" Core..=) Core.<$> description,
            ("quota" Core..=) Core.<$> quota,
            ("throttle" Core..=) Core.<$> throttle,
            Core.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath CreateUsagePlan where
  toPath = Core.const "/usageplans"

instance Core.ToQuery CreateUsagePlan where
  toQuery = Core.const Core.mempty
