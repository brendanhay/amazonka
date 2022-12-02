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
-- Module      : Amazonka.CloudDirectory.Types.BatchReadOperation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchReadOperation where

import Amazonka.CloudDirectory.Types.BatchGetLinkAttributes
import Amazonka.CloudDirectory.Types.BatchGetObjectAttributes
import Amazonka.CloudDirectory.Types.BatchGetObjectInformation
import Amazonka.CloudDirectory.Types.BatchListAttachedIndices
import Amazonka.CloudDirectory.Types.BatchListIncomingTypedLinks
import Amazonka.CloudDirectory.Types.BatchListIndex
import Amazonka.CloudDirectory.Types.BatchListObjectAttributes
import Amazonka.CloudDirectory.Types.BatchListObjectChildren
import Amazonka.CloudDirectory.Types.BatchListObjectParentPaths
import Amazonka.CloudDirectory.Types.BatchListObjectParents
import Amazonka.CloudDirectory.Types.BatchListObjectPolicies
import Amazonka.CloudDirectory.Types.BatchListOutgoingTypedLinks
import Amazonka.CloudDirectory.Types.BatchListPolicyAttachments
import Amazonka.CloudDirectory.Types.BatchLookupPolicy
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a @BatchRead@ operation.
--
-- /See:/ 'newBatchReadOperation' smart constructor.
data BatchReadOperation = BatchReadOperation'
  { -- | Returns a paginated list of all the outgoing TypedLinkSpecifier
    -- information for an object. It also supports filtering by typed link
    -- facet and identity attributes. For more information, see
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
    listOutgoingTypedLinks :: Prelude.Maybe BatchListOutgoingTypedLinks,
    -- | Retrieves attributes within a facet that are associated with an object.
    getObjectAttributes :: Prelude.Maybe BatchGetObjectAttributes,
    -- | Returns a paginated list of child objects that are associated with a
    -- given object.
    listObjectChildren :: Prelude.Maybe BatchListObjectChildren,
    -- | Returns all of the @ObjectIdentifiers@ to which a given policy is
    -- attached.
    listPolicyAttachments :: Prelude.Maybe BatchListPolicyAttachments,
    -- | Returns policies attached to an object in pagination fashion.
    listObjectPolicies :: Prelude.Maybe BatchListObjectPolicies,
    -- | Returns a paginated list of all the incoming TypedLinkSpecifier
    -- information for an object. It also supports filtering by typed link
    -- facet and identity attributes. For more information, see
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
    listIncomingTypedLinks :: Prelude.Maybe BatchListIncomingTypedLinks,
    -- | Retrieves metadata about an object.
    getObjectInformation :: Prelude.Maybe BatchGetObjectInformation,
    -- | Lists all attributes that are associated with an object.
    listObjectAttributes :: Prelude.Maybe BatchListObjectAttributes,
    -- | Lists all policies from the root of the Directory to the object
    -- specified. If there are no policies present, an empty list is returned.
    -- If policies are present, and if some objects don\'t have the policies
    -- attached, it returns the @ObjectIdentifier@ for such objects. If
    -- policies are present, it returns @ObjectIdentifier@, @policyId@, and
    -- @policyType@. Paths that don\'t lead to the root from the target object
    -- are ignored. For more information, see
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies>.
    lookupPolicy :: Prelude.Maybe BatchLookupPolicy,
    -- | Retrieves attributes that are associated with a typed link.
    getLinkAttributes :: Prelude.Maybe BatchGetLinkAttributes,
    -- | Lists parent objects that are associated with a given object in
    -- pagination fashion.
    listObjectParents :: Prelude.Maybe BatchListObjectParents,
    -- | Retrieves all available parent paths for any object type such as node,
    -- leaf node, policy node, and index node objects. For more information
    -- about objects, see
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directorystructure.html Directory Structure>.
    listObjectParentPaths :: Prelude.Maybe BatchListObjectParentPaths,
    -- | Lists indices attached to an object.
    listAttachedIndices :: Prelude.Maybe BatchListAttachedIndices,
    -- | Lists objects attached to the specified index.
    listIndex :: Prelude.Maybe BatchListIndex
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchReadOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listOutgoingTypedLinks', 'batchReadOperation_listOutgoingTypedLinks' - Returns a paginated list of all the outgoing TypedLinkSpecifier
-- information for an object. It also supports filtering by typed link
-- facet and identity attributes. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
--
-- 'getObjectAttributes', 'batchReadOperation_getObjectAttributes' - Retrieves attributes within a facet that are associated with an object.
--
-- 'listObjectChildren', 'batchReadOperation_listObjectChildren' - Returns a paginated list of child objects that are associated with a
-- given object.
--
-- 'listPolicyAttachments', 'batchReadOperation_listPolicyAttachments' - Returns all of the @ObjectIdentifiers@ to which a given policy is
-- attached.
--
-- 'listObjectPolicies', 'batchReadOperation_listObjectPolicies' - Returns policies attached to an object in pagination fashion.
--
-- 'listIncomingTypedLinks', 'batchReadOperation_listIncomingTypedLinks' - Returns a paginated list of all the incoming TypedLinkSpecifier
-- information for an object. It also supports filtering by typed link
-- facet and identity attributes. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
--
-- 'getObjectInformation', 'batchReadOperation_getObjectInformation' - Retrieves metadata about an object.
--
-- 'listObjectAttributes', 'batchReadOperation_listObjectAttributes' - Lists all attributes that are associated with an object.
--
-- 'lookupPolicy', 'batchReadOperation_lookupPolicy' - Lists all policies from the root of the Directory to the object
-- specified. If there are no policies present, an empty list is returned.
-- If policies are present, and if some objects don\'t have the policies
-- attached, it returns the @ObjectIdentifier@ for such objects. If
-- policies are present, it returns @ObjectIdentifier@, @policyId@, and
-- @policyType@. Paths that don\'t lead to the root from the target object
-- are ignored. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies>.
--
-- 'getLinkAttributes', 'batchReadOperation_getLinkAttributes' - Retrieves attributes that are associated with a typed link.
--
-- 'listObjectParents', 'batchReadOperation_listObjectParents' - Lists parent objects that are associated with a given object in
-- pagination fashion.
--
-- 'listObjectParentPaths', 'batchReadOperation_listObjectParentPaths' - Retrieves all available parent paths for any object type such as node,
-- leaf node, policy node, and index node objects. For more information
-- about objects, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directorystructure.html Directory Structure>.
--
-- 'listAttachedIndices', 'batchReadOperation_listAttachedIndices' - Lists indices attached to an object.
--
-- 'listIndex', 'batchReadOperation_listIndex' - Lists objects attached to the specified index.
newBatchReadOperation ::
  BatchReadOperation
newBatchReadOperation =
  BatchReadOperation'
    { listOutgoingTypedLinks =
        Prelude.Nothing,
      getObjectAttributes = Prelude.Nothing,
      listObjectChildren = Prelude.Nothing,
      listPolicyAttachments = Prelude.Nothing,
      listObjectPolicies = Prelude.Nothing,
      listIncomingTypedLinks = Prelude.Nothing,
      getObjectInformation = Prelude.Nothing,
      listObjectAttributes = Prelude.Nothing,
      lookupPolicy = Prelude.Nothing,
      getLinkAttributes = Prelude.Nothing,
      listObjectParents = Prelude.Nothing,
      listObjectParentPaths = Prelude.Nothing,
      listAttachedIndices = Prelude.Nothing,
      listIndex = Prelude.Nothing
    }

-- | Returns a paginated list of all the outgoing TypedLinkSpecifier
-- information for an object. It also supports filtering by typed link
-- facet and identity attributes. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
batchReadOperation_listOutgoingTypedLinks :: Lens.Lens' BatchReadOperation (Prelude.Maybe BatchListOutgoingTypedLinks)
batchReadOperation_listOutgoingTypedLinks = Lens.lens (\BatchReadOperation' {listOutgoingTypedLinks} -> listOutgoingTypedLinks) (\s@BatchReadOperation' {} a -> s {listOutgoingTypedLinks = a} :: BatchReadOperation)

-- | Retrieves attributes within a facet that are associated with an object.
batchReadOperation_getObjectAttributes :: Lens.Lens' BatchReadOperation (Prelude.Maybe BatchGetObjectAttributes)
batchReadOperation_getObjectAttributes = Lens.lens (\BatchReadOperation' {getObjectAttributes} -> getObjectAttributes) (\s@BatchReadOperation' {} a -> s {getObjectAttributes = a} :: BatchReadOperation)

-- | Returns a paginated list of child objects that are associated with a
-- given object.
batchReadOperation_listObjectChildren :: Lens.Lens' BatchReadOperation (Prelude.Maybe BatchListObjectChildren)
batchReadOperation_listObjectChildren = Lens.lens (\BatchReadOperation' {listObjectChildren} -> listObjectChildren) (\s@BatchReadOperation' {} a -> s {listObjectChildren = a} :: BatchReadOperation)

-- | Returns all of the @ObjectIdentifiers@ to which a given policy is
-- attached.
batchReadOperation_listPolicyAttachments :: Lens.Lens' BatchReadOperation (Prelude.Maybe BatchListPolicyAttachments)
batchReadOperation_listPolicyAttachments = Lens.lens (\BatchReadOperation' {listPolicyAttachments} -> listPolicyAttachments) (\s@BatchReadOperation' {} a -> s {listPolicyAttachments = a} :: BatchReadOperation)

-- | Returns policies attached to an object in pagination fashion.
batchReadOperation_listObjectPolicies :: Lens.Lens' BatchReadOperation (Prelude.Maybe BatchListObjectPolicies)
batchReadOperation_listObjectPolicies = Lens.lens (\BatchReadOperation' {listObjectPolicies} -> listObjectPolicies) (\s@BatchReadOperation' {} a -> s {listObjectPolicies = a} :: BatchReadOperation)

-- | Returns a paginated list of all the incoming TypedLinkSpecifier
-- information for an object. It also supports filtering by typed link
-- facet and identity attributes. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
batchReadOperation_listIncomingTypedLinks :: Lens.Lens' BatchReadOperation (Prelude.Maybe BatchListIncomingTypedLinks)
batchReadOperation_listIncomingTypedLinks = Lens.lens (\BatchReadOperation' {listIncomingTypedLinks} -> listIncomingTypedLinks) (\s@BatchReadOperation' {} a -> s {listIncomingTypedLinks = a} :: BatchReadOperation)

-- | Retrieves metadata about an object.
batchReadOperation_getObjectInformation :: Lens.Lens' BatchReadOperation (Prelude.Maybe BatchGetObjectInformation)
batchReadOperation_getObjectInformation = Lens.lens (\BatchReadOperation' {getObjectInformation} -> getObjectInformation) (\s@BatchReadOperation' {} a -> s {getObjectInformation = a} :: BatchReadOperation)

-- | Lists all attributes that are associated with an object.
batchReadOperation_listObjectAttributes :: Lens.Lens' BatchReadOperation (Prelude.Maybe BatchListObjectAttributes)
batchReadOperation_listObjectAttributes = Lens.lens (\BatchReadOperation' {listObjectAttributes} -> listObjectAttributes) (\s@BatchReadOperation' {} a -> s {listObjectAttributes = a} :: BatchReadOperation)

-- | Lists all policies from the root of the Directory to the object
-- specified. If there are no policies present, an empty list is returned.
-- If policies are present, and if some objects don\'t have the policies
-- attached, it returns the @ObjectIdentifier@ for such objects. If
-- policies are present, it returns @ObjectIdentifier@, @policyId@, and
-- @policyType@. Paths that don\'t lead to the root from the target object
-- are ignored. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies>.
batchReadOperation_lookupPolicy :: Lens.Lens' BatchReadOperation (Prelude.Maybe BatchLookupPolicy)
batchReadOperation_lookupPolicy = Lens.lens (\BatchReadOperation' {lookupPolicy} -> lookupPolicy) (\s@BatchReadOperation' {} a -> s {lookupPolicy = a} :: BatchReadOperation)

-- | Retrieves attributes that are associated with a typed link.
batchReadOperation_getLinkAttributes :: Lens.Lens' BatchReadOperation (Prelude.Maybe BatchGetLinkAttributes)
batchReadOperation_getLinkAttributes = Lens.lens (\BatchReadOperation' {getLinkAttributes} -> getLinkAttributes) (\s@BatchReadOperation' {} a -> s {getLinkAttributes = a} :: BatchReadOperation)

-- | Lists parent objects that are associated with a given object in
-- pagination fashion.
batchReadOperation_listObjectParents :: Lens.Lens' BatchReadOperation (Prelude.Maybe BatchListObjectParents)
batchReadOperation_listObjectParents = Lens.lens (\BatchReadOperation' {listObjectParents} -> listObjectParents) (\s@BatchReadOperation' {} a -> s {listObjectParents = a} :: BatchReadOperation)

-- | Retrieves all available parent paths for any object type such as node,
-- leaf node, policy node, and index node objects. For more information
-- about objects, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directorystructure.html Directory Structure>.
batchReadOperation_listObjectParentPaths :: Lens.Lens' BatchReadOperation (Prelude.Maybe BatchListObjectParentPaths)
batchReadOperation_listObjectParentPaths = Lens.lens (\BatchReadOperation' {listObjectParentPaths} -> listObjectParentPaths) (\s@BatchReadOperation' {} a -> s {listObjectParentPaths = a} :: BatchReadOperation)

-- | Lists indices attached to an object.
batchReadOperation_listAttachedIndices :: Lens.Lens' BatchReadOperation (Prelude.Maybe BatchListAttachedIndices)
batchReadOperation_listAttachedIndices = Lens.lens (\BatchReadOperation' {listAttachedIndices} -> listAttachedIndices) (\s@BatchReadOperation' {} a -> s {listAttachedIndices = a} :: BatchReadOperation)

-- | Lists objects attached to the specified index.
batchReadOperation_listIndex :: Lens.Lens' BatchReadOperation (Prelude.Maybe BatchListIndex)
batchReadOperation_listIndex = Lens.lens (\BatchReadOperation' {listIndex} -> listIndex) (\s@BatchReadOperation' {} a -> s {listIndex = a} :: BatchReadOperation)

instance Prelude.Hashable BatchReadOperation where
  hashWithSalt _salt BatchReadOperation' {..} =
    _salt `Prelude.hashWithSalt` listOutgoingTypedLinks
      `Prelude.hashWithSalt` getObjectAttributes
      `Prelude.hashWithSalt` listObjectChildren
      `Prelude.hashWithSalt` listPolicyAttachments
      `Prelude.hashWithSalt` listObjectPolicies
      `Prelude.hashWithSalt` listIncomingTypedLinks
      `Prelude.hashWithSalt` getObjectInformation
      `Prelude.hashWithSalt` listObjectAttributes
      `Prelude.hashWithSalt` lookupPolicy
      `Prelude.hashWithSalt` getLinkAttributes
      `Prelude.hashWithSalt` listObjectParents
      `Prelude.hashWithSalt` listObjectParentPaths
      `Prelude.hashWithSalt` listAttachedIndices
      `Prelude.hashWithSalt` listIndex

instance Prelude.NFData BatchReadOperation where
  rnf BatchReadOperation' {..} =
    Prelude.rnf listOutgoingTypedLinks
      `Prelude.seq` Prelude.rnf getObjectAttributes
      `Prelude.seq` Prelude.rnf listObjectChildren
      `Prelude.seq` Prelude.rnf listPolicyAttachments
      `Prelude.seq` Prelude.rnf listObjectPolicies
      `Prelude.seq` Prelude.rnf listIncomingTypedLinks
      `Prelude.seq` Prelude.rnf getObjectInformation
      `Prelude.seq` Prelude.rnf listObjectAttributes
      `Prelude.seq` Prelude.rnf lookupPolicy
      `Prelude.seq` Prelude.rnf getLinkAttributes
      `Prelude.seq` Prelude.rnf listObjectParents
      `Prelude.seq` Prelude.rnf listObjectParentPaths
      `Prelude.seq` Prelude.rnf listAttachedIndices
      `Prelude.seq` Prelude.rnf listIndex

instance Data.ToJSON BatchReadOperation where
  toJSON BatchReadOperation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ListOutgoingTypedLinks" Data..=)
              Prelude.<$> listOutgoingTypedLinks,
            ("GetObjectAttributes" Data..=)
              Prelude.<$> getObjectAttributes,
            ("ListObjectChildren" Data..=)
              Prelude.<$> listObjectChildren,
            ("ListPolicyAttachments" Data..=)
              Prelude.<$> listPolicyAttachments,
            ("ListObjectPolicies" Data..=)
              Prelude.<$> listObjectPolicies,
            ("ListIncomingTypedLinks" Data..=)
              Prelude.<$> listIncomingTypedLinks,
            ("GetObjectInformation" Data..=)
              Prelude.<$> getObjectInformation,
            ("ListObjectAttributes" Data..=)
              Prelude.<$> listObjectAttributes,
            ("LookupPolicy" Data..=) Prelude.<$> lookupPolicy,
            ("GetLinkAttributes" Data..=)
              Prelude.<$> getLinkAttributes,
            ("ListObjectParents" Data..=)
              Prelude.<$> listObjectParents,
            ("ListObjectParentPaths" Data..=)
              Prelude.<$> listObjectParentPaths,
            ("ListAttachedIndices" Data..=)
              Prelude.<$> listAttachedIndices,
            ("ListIndex" Data..=) Prelude.<$> listIndex
          ]
      )
