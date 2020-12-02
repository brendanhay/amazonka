{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.OriginApprovalRuleTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.OriginApprovalRuleTemplate where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about the template that created the approval rule for a pull request.
--
--
--
-- /See:/ 'originApprovalRuleTemplate' smart constructor.
data OriginApprovalRuleTemplate = OriginApprovalRuleTemplate'
  { _oartApprovalRuleTemplateId ::
      !(Maybe Text),
    _oartApprovalRuleTemplateName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OriginApprovalRuleTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oartApprovalRuleTemplateId' - The ID of the template that created the approval rule.
--
-- * 'oartApprovalRuleTemplateName' - The name of the template that created the approval rule.
originApprovalRuleTemplate ::
  OriginApprovalRuleTemplate
originApprovalRuleTemplate =
  OriginApprovalRuleTemplate'
    { _oartApprovalRuleTemplateId =
        Nothing,
      _oartApprovalRuleTemplateName = Nothing
    }

-- | The ID of the template that created the approval rule.
oartApprovalRuleTemplateId :: Lens' OriginApprovalRuleTemplate (Maybe Text)
oartApprovalRuleTemplateId = lens _oartApprovalRuleTemplateId (\s a -> s {_oartApprovalRuleTemplateId = a})

-- | The name of the template that created the approval rule.
oartApprovalRuleTemplateName :: Lens' OriginApprovalRuleTemplate (Maybe Text)
oartApprovalRuleTemplateName = lens _oartApprovalRuleTemplateName (\s a -> s {_oartApprovalRuleTemplateName = a})

instance FromJSON OriginApprovalRuleTemplate where
  parseJSON =
    withObject
      "OriginApprovalRuleTemplate"
      ( \x ->
          OriginApprovalRuleTemplate'
            <$> (x .:? "approvalRuleTemplateId")
            <*> (x .:? "approvalRuleTemplateName")
      )

instance Hashable OriginApprovalRuleTemplate

instance NFData OriginApprovalRuleTemplate
