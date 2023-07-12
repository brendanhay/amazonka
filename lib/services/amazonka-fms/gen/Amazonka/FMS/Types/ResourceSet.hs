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
-- Module      : Amazonka.FMS.Types.ResourceSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.ResourceSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A set of resources to include in a policy.
--
-- /See:/ 'newResourceSet' smart constructor.
data ResourceSet = ResourceSet'
  { -- | A description of the resource set.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the resource set. This ID is returned in the
    -- responses to create and list commands. You provide it to operations like
    -- update and delete.
    id :: Prelude.Maybe Prelude.Text,
    -- | The last time that the resource set was changed.
    lastUpdateTime :: Prelude.Maybe Data.POSIX,
    -- | An optional token that you can use for optimistic locking. Firewall
    -- Manager returns a token to your requests that access the resource set.
    -- The token marks the state of the resource set resource at the time of
    -- the request. Update tokens are not allowed when creating a resource set.
    -- After creation, each subsequent update call to the resource set requires
    -- the update token.
    --
    -- To make an unconditional change to the resource set, omit the token in
    -- your update request. Without the token, Firewall Manager performs your
    -- updates regardless of whether the resource set has changed since you
    -- last retrieved it.
    --
    -- To make a conditional change to the resource set, provide the token in
    -- your update request. Firewall Manager uses the token to ensure that the
    -- resource set hasn\'t changed since you last retrieved it. If it has
    -- changed, the operation fails with an @InvalidTokenException@. If this
    -- happens, retrieve the resource set again to get a current copy of it
    -- with a new token. Reapply your changes as needed, then try the operation
    -- again using the new token.
    updateToken :: Prelude.Maybe Prelude.Text,
    -- | The descriptive name of the resource set. You can\'t change the name of
    -- a resource set after you create it.
    name :: Prelude.Text,
    -- | Determines the resources that can be associated to the resource set.
    -- Depending on your setting for max results and the number of resource
    -- sets, a single call might not return the full list.
    resourceTypeList :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'resourceSet_description' - A description of the resource set.
--
-- 'id', 'resourceSet_id' - A unique identifier for the resource set. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
--
-- 'lastUpdateTime', 'resourceSet_lastUpdateTime' - The last time that the resource set was changed.
--
-- 'updateToken', 'resourceSet_updateToken' - An optional token that you can use for optimistic locking. Firewall
-- Manager returns a token to your requests that access the resource set.
-- The token marks the state of the resource set resource at the time of
-- the request. Update tokens are not allowed when creating a resource set.
-- After creation, each subsequent update call to the resource set requires
-- the update token.
--
-- To make an unconditional change to the resource set, omit the token in
-- your update request. Without the token, Firewall Manager performs your
-- updates regardless of whether the resource set has changed since you
-- last retrieved it.
--
-- To make a conditional change to the resource set, provide the token in
-- your update request. Firewall Manager uses the token to ensure that the
-- resource set hasn\'t changed since you last retrieved it. If it has
-- changed, the operation fails with an @InvalidTokenException@. If this
-- happens, retrieve the resource set again to get a current copy of it
-- with a new token. Reapply your changes as needed, then try the operation
-- again using the new token.
--
-- 'name', 'resourceSet_name' - The descriptive name of the resource set. You can\'t change the name of
-- a resource set after you create it.
--
-- 'resourceTypeList', 'resourceSet_resourceTypeList' - Determines the resources that can be associated to the resource set.
-- Depending on your setting for max results and the number of resource
-- sets, a single call might not return the full list.
newResourceSet ::
  -- | 'name'
  Prelude.Text ->
  ResourceSet
newResourceSet pName_ =
  ResourceSet'
    { description = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      updateToken = Prelude.Nothing,
      name = pName_,
      resourceTypeList = Prelude.mempty
    }

-- | A description of the resource set.
resourceSet_description :: Lens.Lens' ResourceSet (Prelude.Maybe Prelude.Text)
resourceSet_description = Lens.lens (\ResourceSet' {description} -> description) (\s@ResourceSet' {} a -> s {description = a} :: ResourceSet)

-- | A unique identifier for the resource set. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
resourceSet_id :: Lens.Lens' ResourceSet (Prelude.Maybe Prelude.Text)
resourceSet_id = Lens.lens (\ResourceSet' {id} -> id) (\s@ResourceSet' {} a -> s {id = a} :: ResourceSet)

-- | The last time that the resource set was changed.
resourceSet_lastUpdateTime :: Lens.Lens' ResourceSet (Prelude.Maybe Prelude.UTCTime)
resourceSet_lastUpdateTime = Lens.lens (\ResourceSet' {lastUpdateTime} -> lastUpdateTime) (\s@ResourceSet' {} a -> s {lastUpdateTime = a} :: ResourceSet) Prelude.. Lens.mapping Data._Time

-- | An optional token that you can use for optimistic locking. Firewall
-- Manager returns a token to your requests that access the resource set.
-- The token marks the state of the resource set resource at the time of
-- the request. Update tokens are not allowed when creating a resource set.
-- After creation, each subsequent update call to the resource set requires
-- the update token.
--
-- To make an unconditional change to the resource set, omit the token in
-- your update request. Without the token, Firewall Manager performs your
-- updates regardless of whether the resource set has changed since you
-- last retrieved it.
--
-- To make a conditional change to the resource set, provide the token in
-- your update request. Firewall Manager uses the token to ensure that the
-- resource set hasn\'t changed since you last retrieved it. If it has
-- changed, the operation fails with an @InvalidTokenException@. If this
-- happens, retrieve the resource set again to get a current copy of it
-- with a new token. Reapply your changes as needed, then try the operation
-- again using the new token.
resourceSet_updateToken :: Lens.Lens' ResourceSet (Prelude.Maybe Prelude.Text)
resourceSet_updateToken = Lens.lens (\ResourceSet' {updateToken} -> updateToken) (\s@ResourceSet' {} a -> s {updateToken = a} :: ResourceSet)

-- | The descriptive name of the resource set. You can\'t change the name of
-- a resource set after you create it.
resourceSet_name :: Lens.Lens' ResourceSet Prelude.Text
resourceSet_name = Lens.lens (\ResourceSet' {name} -> name) (\s@ResourceSet' {} a -> s {name = a} :: ResourceSet)

-- | Determines the resources that can be associated to the resource set.
-- Depending on your setting for max results and the number of resource
-- sets, a single call might not return the full list.
resourceSet_resourceTypeList :: Lens.Lens' ResourceSet [Prelude.Text]
resourceSet_resourceTypeList = Lens.lens (\ResourceSet' {resourceTypeList} -> resourceTypeList) (\s@ResourceSet' {} a -> s {resourceTypeList = a} :: ResourceSet) Prelude.. Lens.coerced

instance Data.FromJSON ResourceSet where
  parseJSON =
    Data.withObject
      "ResourceSet"
      ( \x ->
          ResourceSet'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LastUpdateTime")
            Prelude.<*> (x Data..:? "UpdateToken")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> ( x
                            Data..:? "ResourceTypeList"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ResourceSet where
  hashWithSalt _salt ResourceSet' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` updateToken
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resourceTypeList

instance Prelude.NFData ResourceSet where
  rnf ResourceSet' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf updateToken
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf resourceTypeList

instance Data.ToJSON ResourceSet where
  toJSON ResourceSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Id" Data..=) Prelude.<$> id,
            ("LastUpdateTime" Data..=)
              Prelude.<$> lastUpdateTime,
            ("UpdateToken" Data..=) Prelude.<$> updateToken,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("ResourceTypeList" Data..= resourceTypeList)
          ]
      )
