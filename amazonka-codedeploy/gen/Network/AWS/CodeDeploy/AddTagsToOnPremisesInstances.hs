{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    instanceNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { tags =
        Prelude.mempty,
      instanceNames = Prelude.mempty
    }

-- | The tag key-value pairs to add to the on-premises instances.
--
-- Keys and values are both required. Keys cannot be null or empty strings.
-- Value-only tags are not allowed.
addTagsToOnPremisesInstances_tags :: Lens.Lens' AddTagsToOnPremisesInstances [Tag]
addTagsToOnPremisesInstances_tags = Lens.lens (\AddTagsToOnPremisesInstances' {tags} -> tags) (\s@AddTagsToOnPremisesInstances' {} a -> s {tags = a} :: AddTagsToOnPremisesInstances) Prelude.. Prelude._Coerce

-- | The names of the on-premises instances to which to add tags.
addTagsToOnPremisesInstances_instanceNames :: Lens.Lens' AddTagsToOnPremisesInstances [Prelude.Text]
addTagsToOnPremisesInstances_instanceNames = Lens.lens (\AddTagsToOnPremisesInstances' {instanceNames} -> instanceNames) (\s@AddTagsToOnPremisesInstances' {} a -> s {instanceNames = a} :: AddTagsToOnPremisesInstances) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    AddTagsToOnPremisesInstances
  where
  type
    Rs AddTagsToOnPremisesInstances =
      AddTagsToOnPremisesInstancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      AddTagsToOnPremisesInstancesResponse'

instance
  Prelude.Hashable
    AddTagsToOnPremisesInstances

instance Prelude.NFData AddTagsToOnPremisesInstances

instance
  Prelude.ToHeaders
    AddTagsToOnPremisesInstances
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeDeploy_20141006.AddTagsToOnPremisesInstances" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AddTagsToOnPremisesInstances where
  toJSON AddTagsToOnPremisesInstances' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("tags" Prelude..= tags),
            Prelude.Just
              ("instanceNames" Prelude..= instanceNames)
          ]
      )

instance Prelude.ToPath AddTagsToOnPremisesInstances where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AddTagsToOnPremisesInstances where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddTagsToOnPremisesInstancesResponse' smart constructor.
data AddTagsToOnPremisesInstancesResponse = AddTagsToOnPremisesInstancesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddTagsToOnPremisesInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAddTagsToOnPremisesInstancesResponse ::
  AddTagsToOnPremisesInstancesResponse
newAddTagsToOnPremisesInstancesResponse =
  AddTagsToOnPremisesInstancesResponse'

instance
  Prelude.NFData
    AddTagsToOnPremisesInstancesResponse
