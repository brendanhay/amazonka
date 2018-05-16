{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.PutComplianceItems
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a compliance type and other compliance details on a designated resource. This action lets you register custom compliance details with a resource. This call overwrites existing compliance information on the resource, so you must provide a full list of compliance items each time that you send the request.
--
--
-- ComplianceType can be one of the following:
--
--     * ExecutionId: The execution ID when the patch, association, or custom compliance item was applied.
--
--     * ExecutionType: Specify patch, association, or Custom:@string@ .
--
--     * ExecutionTime. The time the patch, association, or custom compliance item was applied to the instance.
--
--     * Id: The patch, association, or custom compliance ID.
--
--     * Title: A title.
--
--     * Status: The status of the compliance item. For example, @approved@ for patches, or @Failed@ for associations.
--
--     * Severity: A patch severity. For example, @critical@ .
--
--     * DocumentName: A SSM document name. For example, AWS-RunPatchBaseline.
--
--     * DocumentVersion: An SSM document version number. For example, 4.
--
--     * Classification: A patch classification. For example, @security updates@ .
--
--     * PatchBaselineId: A patch baseline ID.
--
--     * PatchSeverity: A patch severity. For example, @Critical@ .
--
--     * PatchState: A patch state. For example, @InstancesWithFailedPatches@ .
--
--     * PatchGroup: The name of a patch group.
--
--     * InstalledTime: The time the association, patch, or custom compliance item was applied to the resource. Specify the time by using the following format: yyyy-MM-dd'T'HH:mm:ss'Z'
--
--
--
module Network.AWS.SSM.PutComplianceItems
    (
    -- * Creating a Request
      putComplianceItems
    , PutComplianceItems
    -- * Request Lenses
    , pciItemContentHash
    , pciResourceId
    , pciResourceType
    , pciComplianceType
    , pciExecutionSummary
    , pciItems

    -- * Destructuring the Response
    , putComplianceItemsResponse
    , PutComplianceItemsResponse
    -- * Response Lenses
    , pcirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'putComplianceItems' smart constructor.
data PutComplianceItems = PutComplianceItems'
  { _pciItemContentHash  :: !(Maybe Text)
  , _pciResourceId       :: !Text
  , _pciResourceType     :: !Text
  , _pciComplianceType   :: !Text
  , _pciExecutionSummary :: !ComplianceExecutionSummary
  , _pciItems            :: ![ComplianceItemEntry]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutComplianceItems' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pciItemContentHash' - MD5 or SHA-256 content hash. The content hash is used to determine if existing information should be overwritten or ignored. If the content hashes match, the request to put compliance information is ignored.
--
-- * 'pciResourceId' - Specify an ID for this resource. For a managed instance, this is the instance ID.
--
-- * 'pciResourceType' - Specify the type of resource. @ManagedInstance@ is currently the only supported resource type.
--
-- * 'pciComplianceType' - Specify the compliance type. For example, specify Association (for a State Manager association), Patch, or Custom:@string@ .
--
-- * 'pciExecutionSummary' - A summary of the call execution that includes an execution ID, the type of execution (for example, @Command@ ), and the date/time of the execution using a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.
--
-- * 'pciItems' - Information about the compliance as defined by the resource type. For example, for a patch compliance type, @Items@ includes information about the PatchSeverity, Classification, etc.
putComplianceItems
    :: Text -- ^ 'pciResourceId'
    -> Text -- ^ 'pciResourceType'
    -> Text -- ^ 'pciComplianceType'
    -> ComplianceExecutionSummary -- ^ 'pciExecutionSummary'
    -> PutComplianceItems
putComplianceItems pResourceId_ pResourceType_ pComplianceType_ pExecutionSummary_ =
  PutComplianceItems'
    { _pciItemContentHash = Nothing
    , _pciResourceId = pResourceId_
    , _pciResourceType = pResourceType_
    , _pciComplianceType = pComplianceType_
    , _pciExecutionSummary = pExecutionSummary_
    , _pciItems = mempty
    }


-- | MD5 or SHA-256 content hash. The content hash is used to determine if existing information should be overwritten or ignored. If the content hashes match, the request to put compliance information is ignored.
pciItemContentHash :: Lens' PutComplianceItems (Maybe Text)
pciItemContentHash = lens _pciItemContentHash (\ s a -> s{_pciItemContentHash = a})

-- | Specify an ID for this resource. For a managed instance, this is the instance ID.
pciResourceId :: Lens' PutComplianceItems Text
pciResourceId = lens _pciResourceId (\ s a -> s{_pciResourceId = a})

-- | Specify the type of resource. @ManagedInstance@ is currently the only supported resource type.
pciResourceType :: Lens' PutComplianceItems Text
pciResourceType = lens _pciResourceType (\ s a -> s{_pciResourceType = a})

-- | Specify the compliance type. For example, specify Association (for a State Manager association), Patch, or Custom:@string@ .
pciComplianceType :: Lens' PutComplianceItems Text
pciComplianceType = lens _pciComplianceType (\ s a -> s{_pciComplianceType = a})

-- | A summary of the call execution that includes an execution ID, the type of execution (for example, @Command@ ), and the date/time of the execution using a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.
pciExecutionSummary :: Lens' PutComplianceItems ComplianceExecutionSummary
pciExecutionSummary = lens _pciExecutionSummary (\ s a -> s{_pciExecutionSummary = a})

-- | Information about the compliance as defined by the resource type. For example, for a patch compliance type, @Items@ includes information about the PatchSeverity, Classification, etc.
pciItems :: Lens' PutComplianceItems [ComplianceItemEntry]
pciItems = lens _pciItems (\ s a -> s{_pciItems = a}) . _Coerce

instance AWSRequest PutComplianceItems where
        type Rs PutComplianceItems =
             PutComplianceItemsResponse
        request = postJSON ssm
        response
          = receiveEmpty
              (\ s h x ->
                 PutComplianceItemsResponse' <$> (pure (fromEnum s)))

instance Hashable PutComplianceItems where

instance NFData PutComplianceItems where

instance ToHeaders PutComplianceItems where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.PutComplianceItems" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutComplianceItems where
        toJSON PutComplianceItems'{..}
          = object
              (catMaybes
                 [("ItemContentHash" .=) <$> _pciItemContentHash,
                  Just ("ResourceId" .= _pciResourceId),
                  Just ("ResourceType" .= _pciResourceType),
                  Just ("ComplianceType" .= _pciComplianceType),
                  Just ("ExecutionSummary" .= _pciExecutionSummary),
                  Just ("Items" .= _pciItems)])

instance ToPath PutComplianceItems where
        toPath = const "/"

instance ToQuery PutComplianceItems where
        toQuery = const mempty

-- | /See:/ 'putComplianceItemsResponse' smart constructor.
newtype PutComplianceItemsResponse = PutComplianceItemsResponse'
  { _pcirsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutComplianceItemsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcirsResponseStatus' - -- | The response status code.
putComplianceItemsResponse
    :: Int -- ^ 'pcirsResponseStatus'
    -> PutComplianceItemsResponse
putComplianceItemsResponse pResponseStatus_ =
  PutComplianceItemsResponse' {_pcirsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
pcirsResponseStatus :: Lens' PutComplianceItemsResponse Int
pcirsResponseStatus = lens _pcirsResponseStatus (\ s a -> s{_pcirsResponseStatus = a})

instance NFData PutComplianceItemsResponse where
