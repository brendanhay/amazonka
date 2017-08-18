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
-- Module      : Network.AWS.SSM.UpdatePatchBaseline
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing patch baseline. Fields not specified in the request are left unchanged.
--
--
module Network.AWS.SSM.UpdatePatchBaseline
    (
    -- * Creating a Request
      updatePatchBaseline
    , UpdatePatchBaseline
    -- * Request Lenses
    , upbApprovalRules
    , upbGlobalFilters
    , upbApprovedPatchesComplianceLevel
    , upbApprovedPatches
    , upbRejectedPatches
    , upbName
    , upbDescription
    , upbBaselineId

    -- * Destructuring the Response
    , updatePatchBaselineResponse
    , UpdatePatchBaselineResponse
    -- * Response Lenses
    , upbrsApprovalRules
    , upbrsOperatingSystem
    , upbrsGlobalFilters
    , upbrsApprovedPatchesComplianceLevel
    , upbrsApprovedPatches
    , upbrsRejectedPatches
    , upbrsCreatedDate
    , upbrsName
    , upbrsModifiedDate
    , upbrsDescription
    , upbrsBaselineId
    , upbrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SSM.Types
import           Network.AWS.SSM.Types.Product

-- | /See:/ 'updatePatchBaseline' smart constructor.
data UpdatePatchBaseline = UpdatePatchBaseline'
    { _upbApprovalRules                  :: !(Maybe PatchRuleGroup)
    , _upbGlobalFilters                  :: !(Maybe PatchFilterGroup)
    , _upbApprovedPatchesComplianceLevel :: !(Maybe PatchComplianceLevel)
    , _upbApprovedPatches                :: !(Maybe [Text])
    , _upbRejectedPatches                :: !(Maybe [Text])
    , _upbName                           :: !(Maybe Text)
    , _upbDescription                    :: !(Maybe Text)
    , _upbBaselineId                     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdatePatchBaseline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upbApprovalRules' - A set of rules used to include patches in the baseline.
--
-- * 'upbGlobalFilters' - A set of global filters used to exclude patches from the baseline.
--
-- * 'upbApprovedPatchesComplianceLevel' - Assigns a new compliance severity level to an existing patch baseline.
--
-- * 'upbApprovedPatches' - A list of explicitly approved patches for the baseline.
--
-- * 'upbRejectedPatches' - A list of explicitly rejected patches for the baseline.
--
-- * 'upbName' - The name of the patch baseline.
--
-- * 'upbDescription' - A description of the patch baseline.
--
-- * 'upbBaselineId' - The ID of the patch baseline to update.
updatePatchBaseline
    :: Text -- ^ 'upbBaselineId'
    -> UpdatePatchBaseline
updatePatchBaseline pBaselineId_ =
    UpdatePatchBaseline'
    { _upbApprovalRules = Nothing
    , _upbGlobalFilters = Nothing
    , _upbApprovedPatchesComplianceLevel = Nothing
    , _upbApprovedPatches = Nothing
    , _upbRejectedPatches = Nothing
    , _upbName = Nothing
    , _upbDescription = Nothing
    , _upbBaselineId = pBaselineId_
    }

-- | A set of rules used to include patches in the baseline.
upbApprovalRules :: Lens' UpdatePatchBaseline (Maybe PatchRuleGroup)
upbApprovalRules = lens _upbApprovalRules (\ s a -> s{_upbApprovalRules = a});

-- | A set of global filters used to exclude patches from the baseline.
upbGlobalFilters :: Lens' UpdatePatchBaseline (Maybe PatchFilterGroup)
upbGlobalFilters = lens _upbGlobalFilters (\ s a -> s{_upbGlobalFilters = a});

-- | Assigns a new compliance severity level to an existing patch baseline.
upbApprovedPatchesComplianceLevel :: Lens' UpdatePatchBaseline (Maybe PatchComplianceLevel)
upbApprovedPatchesComplianceLevel = lens _upbApprovedPatchesComplianceLevel (\ s a -> s{_upbApprovedPatchesComplianceLevel = a});

-- | A list of explicitly approved patches for the baseline.
upbApprovedPatches :: Lens' UpdatePatchBaseline [Text]
upbApprovedPatches = lens _upbApprovedPatches (\ s a -> s{_upbApprovedPatches = a}) . _Default . _Coerce;

-- | A list of explicitly rejected patches for the baseline.
upbRejectedPatches :: Lens' UpdatePatchBaseline [Text]
upbRejectedPatches = lens _upbRejectedPatches (\ s a -> s{_upbRejectedPatches = a}) . _Default . _Coerce;

-- | The name of the patch baseline.
upbName :: Lens' UpdatePatchBaseline (Maybe Text)
upbName = lens _upbName (\ s a -> s{_upbName = a});

-- | A description of the patch baseline.
upbDescription :: Lens' UpdatePatchBaseline (Maybe Text)
upbDescription = lens _upbDescription (\ s a -> s{_upbDescription = a});

-- | The ID of the patch baseline to update.
upbBaselineId :: Lens' UpdatePatchBaseline Text
upbBaselineId = lens _upbBaselineId (\ s a -> s{_upbBaselineId = a});

instance AWSRequest UpdatePatchBaseline where
        type Rs UpdatePatchBaseline =
             UpdatePatchBaselineResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 UpdatePatchBaselineResponse' <$>
                   (x .?> "ApprovalRules") <*> (x .?> "OperatingSystem")
                     <*> (x .?> "GlobalFilters")
                     <*> (x .?> "ApprovedPatchesComplianceLevel")
                     <*> (x .?> "ApprovedPatches" .!@ mempty)
                     <*> (x .?> "RejectedPatches" .!@ mempty)
                     <*> (x .?> "CreatedDate")
                     <*> (x .?> "Name")
                     <*> (x .?> "ModifiedDate")
                     <*> (x .?> "Description")
                     <*> (x .?> "BaselineId")
                     <*> (pure (fromEnum s)))

instance Hashable UpdatePatchBaseline

instance NFData UpdatePatchBaseline

instance ToHeaders UpdatePatchBaseline where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.UpdatePatchBaseline" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdatePatchBaseline where
        toJSON UpdatePatchBaseline'{..}
          = object
              (catMaybes
                 [("ApprovalRules" .=) <$> _upbApprovalRules,
                  ("GlobalFilters" .=) <$> _upbGlobalFilters,
                  ("ApprovedPatchesComplianceLevel" .=) <$>
                    _upbApprovedPatchesComplianceLevel,
                  ("ApprovedPatches" .=) <$> _upbApprovedPatches,
                  ("RejectedPatches" .=) <$> _upbRejectedPatches,
                  ("Name" .=) <$> _upbName,
                  ("Description" .=) <$> _upbDescription,
                  Just ("BaselineId" .= _upbBaselineId)])

instance ToPath UpdatePatchBaseline where
        toPath = const "/"

instance ToQuery UpdatePatchBaseline where
        toQuery = const mempty

-- | /See:/ 'updatePatchBaselineResponse' smart constructor.
data UpdatePatchBaselineResponse = UpdatePatchBaselineResponse'
    { _upbrsApprovalRules                  :: !(Maybe PatchRuleGroup)
    , _upbrsOperatingSystem                :: !(Maybe OperatingSystem)
    , _upbrsGlobalFilters                  :: !(Maybe PatchFilterGroup)
    , _upbrsApprovedPatchesComplianceLevel :: !(Maybe PatchComplianceLevel)
    , _upbrsApprovedPatches                :: !(Maybe [Text])
    , _upbrsRejectedPatches                :: !(Maybe [Text])
    , _upbrsCreatedDate                    :: !(Maybe POSIX)
    , _upbrsName                           :: !(Maybe Text)
    , _upbrsModifiedDate                   :: !(Maybe POSIX)
    , _upbrsDescription                    :: !(Maybe Text)
    , _upbrsBaselineId                     :: !(Maybe Text)
    , _upbrsResponseStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdatePatchBaselineResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upbrsApprovalRules' - A set of rules used to include patches in the baseline.
--
-- * 'upbrsOperatingSystem' - The operating system rule used by the updated patch baseline.
--
-- * 'upbrsGlobalFilters' - A set of global filters used to exclude patches from the baseline.
--
-- * 'upbrsApprovedPatchesComplianceLevel' - The compliance severity level assigned to the patch baseline after the update completed.
--
-- * 'upbrsApprovedPatches' - A list of explicitly approved patches for the baseline.
--
-- * 'upbrsRejectedPatches' - A list of explicitly rejected patches for the baseline.
--
-- * 'upbrsCreatedDate' - The date when the patch baseline was created.
--
-- * 'upbrsName' - The name of the patch baseline.
--
-- * 'upbrsModifiedDate' - The date when the patch baseline was last modified.
--
-- * 'upbrsDescription' - A description of the Patch Baseline.
--
-- * 'upbrsBaselineId' - The ID of the deleted patch baseline.
--
-- * 'upbrsResponseStatus' - -- | The response status code.
updatePatchBaselineResponse
    :: Int -- ^ 'upbrsResponseStatus'
    -> UpdatePatchBaselineResponse
updatePatchBaselineResponse pResponseStatus_ =
    UpdatePatchBaselineResponse'
    { _upbrsApprovalRules = Nothing
    , _upbrsOperatingSystem = Nothing
    , _upbrsGlobalFilters = Nothing
    , _upbrsApprovedPatchesComplianceLevel = Nothing
    , _upbrsApprovedPatches = Nothing
    , _upbrsRejectedPatches = Nothing
    , _upbrsCreatedDate = Nothing
    , _upbrsName = Nothing
    , _upbrsModifiedDate = Nothing
    , _upbrsDescription = Nothing
    , _upbrsBaselineId = Nothing
    , _upbrsResponseStatus = pResponseStatus_
    }

-- | A set of rules used to include patches in the baseline.
upbrsApprovalRules :: Lens' UpdatePatchBaselineResponse (Maybe PatchRuleGroup)
upbrsApprovalRules = lens _upbrsApprovalRules (\ s a -> s{_upbrsApprovalRules = a});

-- | The operating system rule used by the updated patch baseline.
upbrsOperatingSystem :: Lens' UpdatePatchBaselineResponse (Maybe OperatingSystem)
upbrsOperatingSystem = lens _upbrsOperatingSystem (\ s a -> s{_upbrsOperatingSystem = a});

-- | A set of global filters used to exclude patches from the baseline.
upbrsGlobalFilters :: Lens' UpdatePatchBaselineResponse (Maybe PatchFilterGroup)
upbrsGlobalFilters = lens _upbrsGlobalFilters (\ s a -> s{_upbrsGlobalFilters = a});

-- | The compliance severity level assigned to the patch baseline after the update completed.
upbrsApprovedPatchesComplianceLevel :: Lens' UpdatePatchBaselineResponse (Maybe PatchComplianceLevel)
upbrsApprovedPatchesComplianceLevel = lens _upbrsApprovedPatchesComplianceLevel (\ s a -> s{_upbrsApprovedPatchesComplianceLevel = a});

-- | A list of explicitly approved patches for the baseline.
upbrsApprovedPatches :: Lens' UpdatePatchBaselineResponse [Text]
upbrsApprovedPatches = lens _upbrsApprovedPatches (\ s a -> s{_upbrsApprovedPatches = a}) . _Default . _Coerce;

-- | A list of explicitly rejected patches for the baseline.
upbrsRejectedPatches :: Lens' UpdatePatchBaselineResponse [Text]
upbrsRejectedPatches = lens _upbrsRejectedPatches (\ s a -> s{_upbrsRejectedPatches = a}) . _Default . _Coerce;

-- | The date when the patch baseline was created.
upbrsCreatedDate :: Lens' UpdatePatchBaselineResponse (Maybe UTCTime)
upbrsCreatedDate = lens _upbrsCreatedDate (\ s a -> s{_upbrsCreatedDate = a}) . mapping _Time;

-- | The name of the patch baseline.
upbrsName :: Lens' UpdatePatchBaselineResponse (Maybe Text)
upbrsName = lens _upbrsName (\ s a -> s{_upbrsName = a});

-- | The date when the patch baseline was last modified.
upbrsModifiedDate :: Lens' UpdatePatchBaselineResponse (Maybe UTCTime)
upbrsModifiedDate = lens _upbrsModifiedDate (\ s a -> s{_upbrsModifiedDate = a}) . mapping _Time;

-- | A description of the Patch Baseline.
upbrsDescription :: Lens' UpdatePatchBaselineResponse (Maybe Text)
upbrsDescription = lens _upbrsDescription (\ s a -> s{_upbrsDescription = a});

-- | The ID of the deleted patch baseline.
upbrsBaselineId :: Lens' UpdatePatchBaselineResponse (Maybe Text)
upbrsBaselineId = lens _upbrsBaselineId (\ s a -> s{_upbrsBaselineId = a});

-- | -- | The response status code.
upbrsResponseStatus :: Lens' UpdatePatchBaselineResponse Int
upbrsResponseStatus = lens _upbrsResponseStatus (\ s a -> s{_upbrsResponseStatus = a});

instance NFData UpdatePatchBaselineResponse
