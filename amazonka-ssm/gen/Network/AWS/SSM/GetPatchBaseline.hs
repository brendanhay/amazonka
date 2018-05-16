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
-- Module      : Network.AWS.SSM.GetPatchBaseline
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a patch baseline.
--
--
module Network.AWS.SSM.GetPatchBaseline
    (
    -- * Creating a Request
      getPatchBaseline
    , GetPatchBaseline
    -- * Request Lenses
    , gpbBaselineId

    -- * Destructuring the Response
    , getPatchBaselineResponse
    , GetPatchBaselineResponse
    -- * Response Lenses
    , gpbrsApprovalRules
    , gpbrsOperatingSystem
    , gpbrsGlobalFilters
    , gpbrsApprovedPatchesComplianceLevel
    , gpbrsApprovedPatches
    , gpbrsApprovedPatchesEnableNonSecurity
    , gpbrsRejectedPatches
    , gpbrsSources
    , gpbrsCreatedDate
    , gpbrsName
    , gpbrsPatchGroups
    , gpbrsModifiedDate
    , gpbrsDescription
    , gpbrsBaselineId
    , gpbrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'getPatchBaseline' smart constructor.
newtype GetPatchBaseline = GetPatchBaseline'
  { _gpbBaselineId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPatchBaseline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpbBaselineId' - The ID of the patch baseline to retrieve.
getPatchBaseline
    :: Text -- ^ 'gpbBaselineId'
    -> GetPatchBaseline
getPatchBaseline pBaselineId_ =
  GetPatchBaseline' {_gpbBaselineId = pBaselineId_}


-- | The ID of the patch baseline to retrieve.
gpbBaselineId :: Lens' GetPatchBaseline Text
gpbBaselineId = lens _gpbBaselineId (\ s a -> s{_gpbBaselineId = a})

instance AWSRequest GetPatchBaseline where
        type Rs GetPatchBaseline = GetPatchBaselineResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 GetPatchBaselineResponse' <$>
                   (x .?> "ApprovalRules") <*> (x .?> "OperatingSystem")
                     <*> (x .?> "GlobalFilters")
                     <*> (x .?> "ApprovedPatchesComplianceLevel")
                     <*> (x .?> "ApprovedPatches" .!@ mempty)
                     <*> (x .?> "ApprovedPatchesEnableNonSecurity")
                     <*> (x .?> "RejectedPatches" .!@ mempty)
                     <*> (x .?> "Sources" .!@ mempty)
                     <*> (x .?> "CreatedDate")
                     <*> (x .?> "Name")
                     <*> (x .?> "PatchGroups" .!@ mempty)
                     <*> (x .?> "ModifiedDate")
                     <*> (x .?> "Description")
                     <*> (x .?> "BaselineId")
                     <*> (pure (fromEnum s)))

instance Hashable GetPatchBaseline where

instance NFData GetPatchBaseline where

instance ToHeaders GetPatchBaseline where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.GetPatchBaseline" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetPatchBaseline where
        toJSON GetPatchBaseline'{..}
          = object
              (catMaybes [Just ("BaselineId" .= _gpbBaselineId)])

instance ToPath GetPatchBaseline where
        toPath = const "/"

instance ToQuery GetPatchBaseline where
        toQuery = const mempty

-- | /See:/ 'getPatchBaselineResponse' smart constructor.
data GetPatchBaselineResponse = GetPatchBaselineResponse'
  { _gpbrsApprovalRules                    :: !(Maybe PatchRuleGroup)
  , _gpbrsOperatingSystem                  :: !(Maybe OperatingSystem)
  , _gpbrsGlobalFilters                    :: !(Maybe PatchFilterGroup)
  , _gpbrsApprovedPatchesComplianceLevel   :: !(Maybe PatchComplianceLevel)
  , _gpbrsApprovedPatches                  :: !(Maybe [Text])
  , _gpbrsApprovedPatchesEnableNonSecurity :: !(Maybe Bool)
  , _gpbrsRejectedPatches                  :: !(Maybe [Text])
  , _gpbrsSources                          :: !(Maybe [PatchSource])
  , _gpbrsCreatedDate                      :: !(Maybe POSIX)
  , _gpbrsName                             :: !(Maybe Text)
  , _gpbrsPatchGroups                      :: !(Maybe [Text])
  , _gpbrsModifiedDate                     :: !(Maybe POSIX)
  , _gpbrsDescription                      :: !(Maybe Text)
  , _gpbrsBaselineId                       :: !(Maybe Text)
  , _gpbrsResponseStatus                   :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPatchBaselineResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpbrsApprovalRules' - A set of rules used to include patches in the baseline.
--
-- * 'gpbrsOperatingSystem' - Returns the operating system specified for the patch baseline.
--
-- * 'gpbrsGlobalFilters' - A set of global filters used to exclude patches from the baseline.
--
-- * 'gpbrsApprovedPatchesComplianceLevel' - Returns the specified compliance severity level for approved patches in the patch baseline.
--
-- * 'gpbrsApprovedPatches' - A list of explicitly approved patches for the baseline.
--
-- * 'gpbrsApprovedPatchesEnableNonSecurity' - Indicates whether the list of approved patches includes non-security updates that should be applied to the instances. The default value is 'false'. Applies to Linux instances only.
--
-- * 'gpbrsRejectedPatches' - A list of explicitly rejected patches for the baseline.
--
-- * 'gpbrsSources' - Information about the patches to use to update the instances, including target operating systems and source repositories. Applies to Linux instances only.
--
-- * 'gpbrsCreatedDate' - The date the patch baseline was created.
--
-- * 'gpbrsName' - The name of the patch baseline.
--
-- * 'gpbrsPatchGroups' - Patch groups included in the patch baseline.
--
-- * 'gpbrsModifiedDate' - The date the patch baseline was last modified.
--
-- * 'gpbrsDescription' - A description of the patch baseline.
--
-- * 'gpbrsBaselineId' - The ID of the retrieved patch baseline.
--
-- * 'gpbrsResponseStatus' - -- | The response status code.
getPatchBaselineResponse
    :: Int -- ^ 'gpbrsResponseStatus'
    -> GetPatchBaselineResponse
getPatchBaselineResponse pResponseStatus_ =
  GetPatchBaselineResponse'
    { _gpbrsApprovalRules = Nothing
    , _gpbrsOperatingSystem = Nothing
    , _gpbrsGlobalFilters = Nothing
    , _gpbrsApprovedPatchesComplianceLevel = Nothing
    , _gpbrsApprovedPatches = Nothing
    , _gpbrsApprovedPatchesEnableNonSecurity = Nothing
    , _gpbrsRejectedPatches = Nothing
    , _gpbrsSources = Nothing
    , _gpbrsCreatedDate = Nothing
    , _gpbrsName = Nothing
    , _gpbrsPatchGroups = Nothing
    , _gpbrsModifiedDate = Nothing
    , _gpbrsDescription = Nothing
    , _gpbrsBaselineId = Nothing
    , _gpbrsResponseStatus = pResponseStatus_
    }


-- | A set of rules used to include patches in the baseline.
gpbrsApprovalRules :: Lens' GetPatchBaselineResponse (Maybe PatchRuleGroup)
gpbrsApprovalRules = lens _gpbrsApprovalRules (\ s a -> s{_gpbrsApprovalRules = a})

-- | Returns the operating system specified for the patch baseline.
gpbrsOperatingSystem :: Lens' GetPatchBaselineResponse (Maybe OperatingSystem)
gpbrsOperatingSystem = lens _gpbrsOperatingSystem (\ s a -> s{_gpbrsOperatingSystem = a})

-- | A set of global filters used to exclude patches from the baseline.
gpbrsGlobalFilters :: Lens' GetPatchBaselineResponse (Maybe PatchFilterGroup)
gpbrsGlobalFilters = lens _gpbrsGlobalFilters (\ s a -> s{_gpbrsGlobalFilters = a})

-- | Returns the specified compliance severity level for approved patches in the patch baseline.
gpbrsApprovedPatchesComplianceLevel :: Lens' GetPatchBaselineResponse (Maybe PatchComplianceLevel)
gpbrsApprovedPatchesComplianceLevel = lens _gpbrsApprovedPatchesComplianceLevel (\ s a -> s{_gpbrsApprovedPatchesComplianceLevel = a})

-- | A list of explicitly approved patches for the baseline.
gpbrsApprovedPatches :: Lens' GetPatchBaselineResponse [Text]
gpbrsApprovedPatches = lens _gpbrsApprovedPatches (\ s a -> s{_gpbrsApprovedPatches = a}) . _Default . _Coerce

-- | Indicates whether the list of approved patches includes non-security updates that should be applied to the instances. The default value is 'false'. Applies to Linux instances only.
gpbrsApprovedPatchesEnableNonSecurity :: Lens' GetPatchBaselineResponse (Maybe Bool)
gpbrsApprovedPatchesEnableNonSecurity = lens _gpbrsApprovedPatchesEnableNonSecurity (\ s a -> s{_gpbrsApprovedPatchesEnableNonSecurity = a})

-- | A list of explicitly rejected patches for the baseline.
gpbrsRejectedPatches :: Lens' GetPatchBaselineResponse [Text]
gpbrsRejectedPatches = lens _gpbrsRejectedPatches (\ s a -> s{_gpbrsRejectedPatches = a}) . _Default . _Coerce

-- | Information about the patches to use to update the instances, including target operating systems and source repositories. Applies to Linux instances only.
gpbrsSources :: Lens' GetPatchBaselineResponse [PatchSource]
gpbrsSources = lens _gpbrsSources (\ s a -> s{_gpbrsSources = a}) . _Default . _Coerce

-- | The date the patch baseline was created.
gpbrsCreatedDate :: Lens' GetPatchBaselineResponse (Maybe UTCTime)
gpbrsCreatedDate = lens _gpbrsCreatedDate (\ s a -> s{_gpbrsCreatedDate = a}) . mapping _Time

-- | The name of the patch baseline.
gpbrsName :: Lens' GetPatchBaselineResponse (Maybe Text)
gpbrsName = lens _gpbrsName (\ s a -> s{_gpbrsName = a})

-- | Patch groups included in the patch baseline.
gpbrsPatchGroups :: Lens' GetPatchBaselineResponse [Text]
gpbrsPatchGroups = lens _gpbrsPatchGroups (\ s a -> s{_gpbrsPatchGroups = a}) . _Default . _Coerce

-- | The date the patch baseline was last modified.
gpbrsModifiedDate :: Lens' GetPatchBaselineResponse (Maybe UTCTime)
gpbrsModifiedDate = lens _gpbrsModifiedDate (\ s a -> s{_gpbrsModifiedDate = a}) . mapping _Time

-- | A description of the patch baseline.
gpbrsDescription :: Lens' GetPatchBaselineResponse (Maybe Text)
gpbrsDescription = lens _gpbrsDescription (\ s a -> s{_gpbrsDescription = a})

-- | The ID of the retrieved patch baseline.
gpbrsBaselineId :: Lens' GetPatchBaselineResponse (Maybe Text)
gpbrsBaselineId = lens _gpbrsBaselineId (\ s a -> s{_gpbrsBaselineId = a})

-- | -- | The response status code.
gpbrsResponseStatus :: Lens' GetPatchBaselineResponse Int
gpbrsResponseStatus = lens _gpbrsResponseStatus (\ s a -> s{_gpbrsResponseStatus = a})

instance NFData GetPatchBaselineResponse where
