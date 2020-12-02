{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ApprovalRuleTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ApprovalRuleTemplate where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about an approval rule template.
--
--
--
-- /See:/ 'approvalRuleTemplate' smart constructor.
data ApprovalRuleTemplate = ApprovalRuleTemplate'
  { _artRuleContentSha256 ::
      !(Maybe Text),
    _artApprovalRuleTemplateId :: !(Maybe Text),
    _artLastModifiedDate :: !(Maybe POSIX),
    _artApprovalRuleTemplateDescription ::
      !(Maybe Text),
    _artApprovalRuleTemplateContent :: !(Maybe Text),
    _artLastModifiedUser :: !(Maybe Text),
    _artCreationDate :: !(Maybe POSIX),
    _artApprovalRuleTemplateName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApprovalRuleTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'artRuleContentSha256' - The SHA-256 hash signature for the content of the approval rule template.
--
-- * 'artApprovalRuleTemplateId' - The system-generated ID of the approval rule template.
--
-- * 'artLastModifiedDate' - The date the approval rule template was most recently changed, in timestamp format.
--
-- * 'artApprovalRuleTemplateDescription' - The description of the approval rule template.
--
-- * 'artApprovalRuleTemplateContent' - The content of the approval rule template.
--
-- * 'artLastModifiedUser' - The Amazon Resource Name (ARN) of the user who made the most recent changes to the approval rule template.
--
-- * 'artCreationDate' - The date the approval rule template was created, in timestamp format.
--
-- * 'artApprovalRuleTemplateName' - The name of the approval rule template.
approvalRuleTemplate ::
  ApprovalRuleTemplate
approvalRuleTemplate =
  ApprovalRuleTemplate'
    { _artRuleContentSha256 = Nothing,
      _artApprovalRuleTemplateId = Nothing,
      _artLastModifiedDate = Nothing,
      _artApprovalRuleTemplateDescription = Nothing,
      _artApprovalRuleTemplateContent = Nothing,
      _artLastModifiedUser = Nothing,
      _artCreationDate = Nothing,
      _artApprovalRuleTemplateName = Nothing
    }

-- | The SHA-256 hash signature for the content of the approval rule template.
artRuleContentSha256 :: Lens' ApprovalRuleTemplate (Maybe Text)
artRuleContentSha256 = lens _artRuleContentSha256 (\s a -> s {_artRuleContentSha256 = a})

-- | The system-generated ID of the approval rule template.
artApprovalRuleTemplateId :: Lens' ApprovalRuleTemplate (Maybe Text)
artApprovalRuleTemplateId = lens _artApprovalRuleTemplateId (\s a -> s {_artApprovalRuleTemplateId = a})

-- | The date the approval rule template was most recently changed, in timestamp format.
artLastModifiedDate :: Lens' ApprovalRuleTemplate (Maybe UTCTime)
artLastModifiedDate = lens _artLastModifiedDate (\s a -> s {_artLastModifiedDate = a}) . mapping _Time

-- | The description of the approval rule template.
artApprovalRuleTemplateDescription :: Lens' ApprovalRuleTemplate (Maybe Text)
artApprovalRuleTemplateDescription = lens _artApprovalRuleTemplateDescription (\s a -> s {_artApprovalRuleTemplateDescription = a})

-- | The content of the approval rule template.
artApprovalRuleTemplateContent :: Lens' ApprovalRuleTemplate (Maybe Text)
artApprovalRuleTemplateContent = lens _artApprovalRuleTemplateContent (\s a -> s {_artApprovalRuleTemplateContent = a})

-- | The Amazon Resource Name (ARN) of the user who made the most recent changes to the approval rule template.
artLastModifiedUser :: Lens' ApprovalRuleTemplate (Maybe Text)
artLastModifiedUser = lens _artLastModifiedUser (\s a -> s {_artLastModifiedUser = a})

-- | The date the approval rule template was created, in timestamp format.
artCreationDate :: Lens' ApprovalRuleTemplate (Maybe UTCTime)
artCreationDate = lens _artCreationDate (\s a -> s {_artCreationDate = a}) . mapping _Time

-- | The name of the approval rule template.
artApprovalRuleTemplateName :: Lens' ApprovalRuleTemplate (Maybe Text)
artApprovalRuleTemplateName = lens _artApprovalRuleTemplateName (\s a -> s {_artApprovalRuleTemplateName = a})

instance FromJSON ApprovalRuleTemplate where
  parseJSON =
    withObject
      "ApprovalRuleTemplate"
      ( \x ->
          ApprovalRuleTemplate'
            <$> (x .:? "ruleContentSha256")
            <*> (x .:? "approvalRuleTemplateId")
            <*> (x .:? "lastModifiedDate")
            <*> (x .:? "approvalRuleTemplateDescription")
            <*> (x .:? "approvalRuleTemplateContent")
            <*> (x .:? "lastModifiedUser")
            <*> (x .:? "creationDate")
            <*> (x .:? "approvalRuleTemplateName")
      )

instance Hashable ApprovalRuleTemplate

instance NFData ApprovalRuleTemplate
