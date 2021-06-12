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
-- Module      : Network.AWS.CodeDeploy.AddTagsToOnPremisesInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to on-premises instances.
module Network.AWS.CodeDeploy.AddTagsToOnPremisesInstances
  ( -- * Creating a Request
    AddTagsToOnPremisesInstances (..),
    newAddTagsToOnPremisesInstances,

    -- * Request Lenses
    addTagsToOnPremisesInstances_tags,
    addTagsToOnPremisesInstances_instanceNames,

    -- * Destructuring the Response
    AddTagsToOnPremisesInstancesResponse (..),
    newAddTagsToOnPremisesInstancesResponse,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of, and adds tags to, an on-premises instance
-- operation.
--
-- /See:/ 'newAddTagsToOnPremisesInstances' smart constructor.
data AddTagsToOnPremisesInstances = AddTagsToOnPremisesInstances'
  { -- | The tag key-value pairs to add to the on-premises instances.
    --
    -- Keys and values are both required. Keys cannot be null or empty strings.
    -- Value-only tags are not allowed.
    tags :: [Tag],
    -- | The names of the on-premises instances to which to add tags.
    instanceNames :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddTagsToOnPremisesInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'addTagsToOnPremisesInstances_tags' - The tag key-value pairs to add to the on-premises instances.
--
-- Keys and values are both required. Keys cannot be null or empty strings.
-- Value-only tags are not allowed.
--
-- 'instanceNames', 'addTagsToOnPremisesInstances_instanceNames' - The names of the on-premises instances to which to add tags.
newAddTagsToOnPremisesInstances ::
  AddTagsToOnPremisesInstances
newAddTagsToOnPremisesInstances =
  AddTagsToOnPremisesInstances'
    { tags = Core.mempty,
      instanceNames = Core.mempty
    }

-- | The tag key-value pairs to add to the on-premises instances.
--
-- Keys and values are both required. Keys cannot be null or empty strings.
-- Value-only tags are not allowed.
addTagsToOnPremisesInstances_tags :: Lens.Lens' AddTagsToOnPremisesInstances [Tag]
addTagsToOnPremisesInstances_tags = Lens.lens (\AddTagsToOnPremisesInstances' {tags} -> tags) (\s@AddTagsToOnPremisesInstances' {} a -> s {tags = a} :: AddTagsToOnPremisesInstances) Core.. Lens._Coerce

-- | The names of the on-premises instances to which to add tags.
addTagsToOnPremisesInstances_instanceNames :: Lens.Lens' AddTagsToOnPremisesInstances [Core.Text]
addTagsToOnPremisesInstances_instanceNames = Lens.lens (\AddTagsToOnPremisesInstances' {instanceNames} -> instanceNames) (\s@AddTagsToOnPremisesInstances' {} a -> s {instanceNames = a} :: AddTagsToOnPremisesInstances) Core.. Lens._Coerce

instance Core.AWSRequest AddTagsToOnPremisesInstances where
  type
    AWSResponse AddTagsToOnPremisesInstances =
      AddTagsToOnPremisesInstancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      AddTagsToOnPremisesInstancesResponse'

instance Core.Hashable AddTagsToOnPremisesInstances

instance Core.NFData AddTagsToOnPremisesInstances

instance Core.ToHeaders AddTagsToOnPremisesInstances where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.AddTagsToOnPremisesInstances" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AddTagsToOnPremisesInstances where
  toJSON AddTagsToOnPremisesInstances' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("tags" Core..= tags),
            Core.Just ("instanceNames" Core..= instanceNames)
          ]
      )

instance Core.ToPath AddTagsToOnPremisesInstances where
  toPath = Core.const "/"

instance Core.ToQuery AddTagsToOnPremisesInstances where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAddTagsToOnPremisesInstancesResponse' smart constructor.
data AddTagsToOnPremisesInstancesResponse = AddTagsToOnPremisesInstancesResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddTagsToOnPremisesInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAddTagsToOnPremisesInstancesResponse ::
  AddTagsToOnPremisesInstancesResponse
newAddTagsToOnPremisesInstancesResponse =
  AddTagsToOnPremisesInstancesResponse'

instance
  Core.NFData
    AddTagsToOnPremisesInstancesResponse
