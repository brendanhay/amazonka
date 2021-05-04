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
-- Module      : Network.AWS.CodeDeploy.RemoveTagsFromOnPremisesInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more tags from one or more on-premises instances.
module Network.AWS.CodeDeploy.RemoveTagsFromOnPremisesInstances
  ( -- * Creating a Request
    RemoveTagsFromOnPremisesInstances (..),
    newRemoveTagsFromOnPremisesInstances,

    -- * Request Lenses
    removeTagsFromOnPremisesInstances_tags,
    removeTagsFromOnPremisesInstances_instanceNames,

    -- * Destructuring the Response
    RemoveTagsFromOnPremisesInstancesResponse (..),
    newRemoveTagsFromOnPremisesInstancesResponse,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @RemoveTagsFromOnPremisesInstances@ operation.
--
-- /See:/ 'newRemoveTagsFromOnPremisesInstances' smart constructor.
data RemoveTagsFromOnPremisesInstances = RemoveTagsFromOnPremisesInstances'
  { -- | The tag key-value pairs to remove from the on-premises instances.
    tags :: [Tag],
    -- | The names of the on-premises instances from which to remove tags.
    instanceNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemoveTagsFromOnPremisesInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'removeTagsFromOnPremisesInstances_tags' - The tag key-value pairs to remove from the on-premises instances.
--
-- 'instanceNames', 'removeTagsFromOnPremisesInstances_instanceNames' - The names of the on-premises instances from which to remove tags.
newRemoveTagsFromOnPremisesInstances ::
  RemoveTagsFromOnPremisesInstances
newRemoveTagsFromOnPremisesInstances =
  RemoveTagsFromOnPremisesInstances'
    { tags =
        Prelude.mempty,
      instanceNames = Prelude.mempty
    }

-- | The tag key-value pairs to remove from the on-premises instances.
removeTagsFromOnPremisesInstances_tags :: Lens.Lens' RemoveTagsFromOnPremisesInstances [Tag]
removeTagsFromOnPremisesInstances_tags = Lens.lens (\RemoveTagsFromOnPremisesInstances' {tags} -> tags) (\s@RemoveTagsFromOnPremisesInstances' {} a -> s {tags = a} :: RemoveTagsFromOnPremisesInstances) Prelude.. Prelude._Coerce

-- | The names of the on-premises instances from which to remove tags.
removeTagsFromOnPremisesInstances_instanceNames :: Lens.Lens' RemoveTagsFromOnPremisesInstances [Prelude.Text]
removeTagsFromOnPremisesInstances_instanceNames = Lens.lens (\RemoveTagsFromOnPremisesInstances' {instanceNames} -> instanceNames) (\s@RemoveTagsFromOnPremisesInstances' {} a -> s {instanceNames = a} :: RemoveTagsFromOnPremisesInstances) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    RemoveTagsFromOnPremisesInstances
  where
  type
    Rs RemoveTagsFromOnPremisesInstances =
      RemoveTagsFromOnPremisesInstancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      RemoveTagsFromOnPremisesInstancesResponse'

instance
  Prelude.Hashable
    RemoveTagsFromOnPremisesInstances

instance
  Prelude.NFData
    RemoveTagsFromOnPremisesInstances

instance
  Prelude.ToHeaders
    RemoveTagsFromOnPremisesInstances
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeDeploy_20141006.RemoveTagsFromOnPremisesInstances" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    RemoveTagsFromOnPremisesInstances
  where
  toJSON RemoveTagsFromOnPremisesInstances' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("tags" Prelude..= tags),
            Prelude.Just
              ("instanceNames" Prelude..= instanceNames)
          ]
      )

instance
  Prelude.ToPath
    RemoveTagsFromOnPremisesInstances
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    RemoveTagsFromOnPremisesInstances
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveTagsFromOnPremisesInstancesResponse' smart constructor.
data RemoveTagsFromOnPremisesInstancesResponse = RemoveTagsFromOnPremisesInstancesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemoveTagsFromOnPremisesInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemoveTagsFromOnPremisesInstancesResponse ::
  RemoveTagsFromOnPremisesInstancesResponse
newRemoveTagsFromOnPremisesInstancesResponse =
  RemoveTagsFromOnPremisesInstancesResponse'

instance
  Prelude.NFData
    RemoveTagsFromOnPremisesInstancesResponse
