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
-- Module      : Network.AWS.SSM.CreatePatchBaseline
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a patch baseline.
--
--
module Network.AWS.SSM.CreatePatchBaseline
    (
    -- * Creating a Request
      createPatchBaseline
    , CreatePatchBaseline
    -- * Request Lenses
    , cpbApprovalRules
    , cpbClientToken
    , cpbOperatingSystem
    , cpbGlobalFilters
    , cpbApprovedPatchesComplianceLevel
    , cpbApprovedPatches
    , cpbApprovedPatchesEnableNonSecurity
    , cpbRejectedPatches
    , cpbSources
    , cpbDescription
    , cpbName

    -- * Destructuring the Response
    , createPatchBaselineResponse
    , CreatePatchBaselineResponse
    -- * Response Lenses
    , cpbrsBaselineId
    , cpbrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'createPatchBaseline' smart constructor.
data CreatePatchBaseline = CreatePatchBaseline'
  { _cpbApprovalRules                    :: !(Maybe PatchRuleGroup)
  , _cpbClientToken                      :: !(Maybe Text)
  , _cpbOperatingSystem                  :: !(Maybe OperatingSystem)
  , _cpbGlobalFilters                    :: !(Maybe PatchFilterGroup)
  , _cpbApprovedPatchesComplianceLevel   :: !(Maybe PatchComplianceLevel)
  , _cpbApprovedPatches                  :: !(Maybe [Text])
  , _cpbApprovedPatchesEnableNonSecurity :: !(Maybe Bool)
  , _cpbRejectedPatches                  :: !(Maybe [Text])
  , _cpbSources                          :: !(Maybe [PatchSource])
  , _cpbDescription                      :: !(Maybe Text)
  , _cpbName                             :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePatchBaseline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpbApprovalRules' - A set of rules used to include patches in the baseline.
--
-- * 'cpbClientToken' - User-provided idempotency token.
--
-- * 'cpbOperatingSystem' - Defines the operating system the patch baseline applies to. The Default value is WINDOWS.
--
-- * 'cpbGlobalFilters' - A set of global filters used to exclude patches from the baseline.
--
-- * 'cpbApprovedPatchesComplianceLevel' - Defines the compliance level for approved patches. This means that if an approved patch is reported as missing, this is the severity of the compliance violation. The default value is UNSPECIFIED.
--
-- * 'cpbApprovedPatches' - A list of explicitly approved patches for the baseline. For information about accepted formats for lists of approved patches and rejected patches, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html Package Name Formats for Approved and Rejected Patch Lists> in the /AWS Systems Manager User Guide/ .
--
-- * 'cpbApprovedPatchesEnableNonSecurity' - Indicates whether the list of approved patches includes non-security updates that should be applied to the instances. The default value is 'false'. Applies to Linux instances only.
--
-- * 'cpbRejectedPatches' - A list of explicitly rejected patches for the baseline. For information about accepted formats for lists of approved patches and rejected patches, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html Package Name Formats for Approved and Rejected Patch Lists> in the /AWS Systems Manager User Guide/ .
--
-- * 'cpbSources' - Information about the patches to use to update the instances, including target operating systems and source repositories. Applies to Linux instances only.
--
-- * 'cpbDescription' - A description of the patch baseline.
--
-- * 'cpbName' - The name of the patch baseline.
createPatchBaseline
    :: Text -- ^ 'cpbName'
    -> CreatePatchBaseline
createPatchBaseline pName_ =
  CreatePatchBaseline'
    { _cpbApprovalRules = Nothing
    , _cpbClientToken = Nothing
    , _cpbOperatingSystem = Nothing
    , _cpbGlobalFilters = Nothing
    , _cpbApprovedPatchesComplianceLevel = Nothing
    , _cpbApprovedPatches = Nothing
    , _cpbApprovedPatchesEnableNonSecurity = Nothing
    , _cpbRejectedPatches = Nothing
    , _cpbSources = Nothing
    , _cpbDescription = Nothing
    , _cpbName = pName_
    }


-- | A set of rules used to include patches in the baseline.
cpbApprovalRules :: Lens' CreatePatchBaseline (Maybe PatchRuleGroup)
cpbApprovalRules = lens _cpbApprovalRules (\ s a -> s{_cpbApprovalRules = a})

-- | User-provided idempotency token.
cpbClientToken :: Lens' CreatePatchBaseline (Maybe Text)
cpbClientToken = lens _cpbClientToken (\ s a -> s{_cpbClientToken = a})

-- | Defines the operating system the patch baseline applies to. The Default value is WINDOWS.
cpbOperatingSystem :: Lens' CreatePatchBaseline (Maybe OperatingSystem)
cpbOperatingSystem = lens _cpbOperatingSystem (\ s a -> s{_cpbOperatingSystem = a})

-- | A set of global filters used to exclude patches from the baseline.
cpbGlobalFilters :: Lens' CreatePatchBaseline (Maybe PatchFilterGroup)
cpbGlobalFilters = lens _cpbGlobalFilters (\ s a -> s{_cpbGlobalFilters = a})

-- | Defines the compliance level for approved patches. This means that if an approved patch is reported as missing, this is the severity of the compliance violation. The default value is UNSPECIFIED.
cpbApprovedPatchesComplianceLevel :: Lens' CreatePatchBaseline (Maybe PatchComplianceLevel)
cpbApprovedPatchesComplianceLevel = lens _cpbApprovedPatchesComplianceLevel (\ s a -> s{_cpbApprovedPatchesComplianceLevel = a})

-- | A list of explicitly approved patches for the baseline. For information about accepted formats for lists of approved patches and rejected patches, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html Package Name Formats for Approved and Rejected Patch Lists> in the /AWS Systems Manager User Guide/ .
cpbApprovedPatches :: Lens' CreatePatchBaseline [Text]
cpbApprovedPatches = lens _cpbApprovedPatches (\ s a -> s{_cpbApprovedPatches = a}) . _Default . _Coerce

-- | Indicates whether the list of approved patches includes non-security updates that should be applied to the instances. The default value is 'false'. Applies to Linux instances only.
cpbApprovedPatchesEnableNonSecurity :: Lens' CreatePatchBaseline (Maybe Bool)
cpbApprovedPatchesEnableNonSecurity = lens _cpbApprovedPatchesEnableNonSecurity (\ s a -> s{_cpbApprovedPatchesEnableNonSecurity = a})

-- | A list of explicitly rejected patches for the baseline. For information about accepted formats for lists of approved patches and rejected patches, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html Package Name Formats for Approved and Rejected Patch Lists> in the /AWS Systems Manager User Guide/ .
cpbRejectedPatches :: Lens' CreatePatchBaseline [Text]
cpbRejectedPatches = lens _cpbRejectedPatches (\ s a -> s{_cpbRejectedPatches = a}) . _Default . _Coerce

-- | Information about the patches to use to update the instances, including target operating systems and source repositories. Applies to Linux instances only.
cpbSources :: Lens' CreatePatchBaseline [PatchSource]
cpbSources = lens _cpbSources (\ s a -> s{_cpbSources = a}) . _Default . _Coerce

-- | A description of the patch baseline.
cpbDescription :: Lens' CreatePatchBaseline (Maybe Text)
cpbDescription = lens _cpbDescription (\ s a -> s{_cpbDescription = a})

-- | The name of the patch baseline.
cpbName :: Lens' CreatePatchBaseline Text
cpbName = lens _cpbName (\ s a -> s{_cpbName = a})

instance AWSRequest CreatePatchBaseline where
        type Rs CreatePatchBaseline =
             CreatePatchBaselineResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 CreatePatchBaselineResponse' <$>
                   (x .?> "BaselineId") <*> (pure (fromEnum s)))

instance Hashable CreatePatchBaseline where

instance NFData CreatePatchBaseline where

instance ToHeaders CreatePatchBaseline where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.CreatePatchBaseline" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreatePatchBaseline where
        toJSON CreatePatchBaseline'{..}
          = object
              (catMaybes
                 [("ApprovalRules" .=) <$> _cpbApprovalRules,
                  ("ClientToken" .=) <$> _cpbClientToken,
                  ("OperatingSystem" .=) <$> _cpbOperatingSystem,
                  ("GlobalFilters" .=) <$> _cpbGlobalFilters,
                  ("ApprovedPatchesComplianceLevel" .=) <$>
                    _cpbApprovedPatchesComplianceLevel,
                  ("ApprovedPatches" .=) <$> _cpbApprovedPatches,
                  ("ApprovedPatchesEnableNonSecurity" .=) <$>
                    _cpbApprovedPatchesEnableNonSecurity,
                  ("RejectedPatches" .=) <$> _cpbRejectedPatches,
                  ("Sources" .=) <$> _cpbSources,
                  ("Description" .=) <$> _cpbDescription,
                  Just ("Name" .= _cpbName)])

instance ToPath CreatePatchBaseline where
        toPath = const "/"

instance ToQuery CreatePatchBaseline where
        toQuery = const mempty

-- | /See:/ 'createPatchBaselineResponse' smart constructor.
data CreatePatchBaselineResponse = CreatePatchBaselineResponse'
  { _cpbrsBaselineId     :: !(Maybe Text)
  , _cpbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePatchBaselineResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpbrsBaselineId' - The ID of the created patch baseline.
--
-- * 'cpbrsResponseStatus' - -- | The response status code.
createPatchBaselineResponse
    :: Int -- ^ 'cpbrsResponseStatus'
    -> CreatePatchBaselineResponse
createPatchBaselineResponse pResponseStatus_ =
  CreatePatchBaselineResponse'
    {_cpbrsBaselineId = Nothing, _cpbrsResponseStatus = pResponseStatus_}


-- | The ID of the created patch baseline.
cpbrsBaselineId :: Lens' CreatePatchBaselineResponse (Maybe Text)
cpbrsBaselineId = lens _cpbrsBaselineId (\ s a -> s{_cpbrsBaselineId = a})

-- | -- | The response status code.
cpbrsResponseStatus :: Lens' CreatePatchBaselineResponse Int
cpbrsResponseStatus = lens _cpbrsResponseStatus (\ s a -> s{_cpbrsResponseStatus = a})

instance NFData CreatePatchBaselineResponse where
