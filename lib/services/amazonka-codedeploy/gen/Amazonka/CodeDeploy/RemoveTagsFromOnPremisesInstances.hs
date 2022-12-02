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
-- Module      : Amazonka.CodeDeploy.RemoveTagsFromOnPremisesInstances
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more tags from one or more on-premises instances.
module Amazonka.CodeDeploy.RemoveTagsFromOnPremisesInstances
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

import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @RemoveTagsFromOnPremisesInstances@ operation.
--
-- /See:/ 'newRemoveTagsFromOnPremisesInstances' smart constructor.
data RemoveTagsFromOnPremisesInstances = RemoveTagsFromOnPremisesInstances'
  { -- | The tag key-value pairs to remove from the on-premises instances.
    tags :: [Tag],
    -- | The names of the on-premises instances from which to remove tags.
    instanceNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
removeTagsFromOnPremisesInstances_tags = Lens.lens (\RemoveTagsFromOnPremisesInstances' {tags} -> tags) (\s@RemoveTagsFromOnPremisesInstances' {} a -> s {tags = a} :: RemoveTagsFromOnPremisesInstances) Prelude.. Lens.coerced

-- | The names of the on-premises instances from which to remove tags.
removeTagsFromOnPremisesInstances_instanceNames :: Lens.Lens' RemoveTagsFromOnPremisesInstances [Prelude.Text]
removeTagsFromOnPremisesInstances_instanceNames = Lens.lens (\RemoveTagsFromOnPremisesInstances' {instanceNames} -> instanceNames) (\s@RemoveTagsFromOnPremisesInstances' {} a -> s {instanceNames = a} :: RemoveTagsFromOnPremisesInstances) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    RemoveTagsFromOnPremisesInstances
  where
  type
    AWSResponse RemoveTagsFromOnPremisesInstances =
      RemoveTagsFromOnPremisesInstancesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      RemoveTagsFromOnPremisesInstancesResponse'

instance
  Prelude.Hashable
    RemoveTagsFromOnPremisesInstances
  where
  hashWithSalt
    _salt
    RemoveTagsFromOnPremisesInstances' {..} =
      _salt `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` instanceNames

instance
  Prelude.NFData
    RemoveTagsFromOnPremisesInstances
  where
  rnf RemoveTagsFromOnPremisesInstances' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf instanceNames

instance
  Data.ToHeaders
    RemoveTagsFromOnPremisesInstances
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeDeploy_20141006.RemoveTagsFromOnPremisesInstances" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    RemoveTagsFromOnPremisesInstances
  where
  toJSON RemoveTagsFromOnPremisesInstances' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("tags" Data..= tags),
            Prelude.Just
              ("instanceNames" Data..= instanceNames)
          ]
      )

instance
  Data.ToPath
    RemoveTagsFromOnPremisesInstances
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    RemoveTagsFromOnPremisesInstances
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveTagsFromOnPremisesInstancesResponse' smart constructor.
data RemoveTagsFromOnPremisesInstancesResponse = RemoveTagsFromOnPremisesInstancesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
