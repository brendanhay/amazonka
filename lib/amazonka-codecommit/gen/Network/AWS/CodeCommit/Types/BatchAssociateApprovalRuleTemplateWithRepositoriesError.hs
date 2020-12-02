{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.BatchAssociateApprovalRuleTemplateWithRepositoriesError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.BatchAssociateApprovalRuleTemplateWithRepositoriesError where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns information about errors in a BatchAssociateApprovalRuleTemplateWithRepositories operation.
--
--
--
-- /See:/ 'batchAssociateApprovalRuleTemplateWithRepositoriesError' smart constructor.
data BatchAssociateApprovalRuleTemplateWithRepositoriesError = BatchAssociateApprovalRuleTemplateWithRepositoriesError'
  { _baartwreErrorCode ::
      !( Maybe
           Text
       ),
    _baartwreRepositoryName ::
      !( Maybe
           Text
       ),
    _baartwreErrorMessage ::
      !( Maybe
           Text
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'BatchAssociateApprovalRuleTemplateWithRepositoriesError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'baartwreErrorCode' - An error code that specifies whether the repository name was not valid or not found.
--
-- * 'baartwreRepositoryName' - The name of the repository where the association was not made.
--
-- * 'baartwreErrorMessage' - An error message that provides details about why the repository name was not found or not valid.
batchAssociateApprovalRuleTemplateWithRepositoriesError ::
  BatchAssociateApprovalRuleTemplateWithRepositoriesError
batchAssociateApprovalRuleTemplateWithRepositoriesError =
  BatchAssociateApprovalRuleTemplateWithRepositoriesError'
    { _baartwreErrorCode =
        Nothing,
      _baartwreRepositoryName = Nothing,
      _baartwreErrorMessage = Nothing
    }

-- | An error code that specifies whether the repository name was not valid or not found.
baartwreErrorCode :: Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesError (Maybe Text)
baartwreErrorCode = lens _baartwreErrorCode (\s a -> s {_baartwreErrorCode = a})

-- | The name of the repository where the association was not made.
baartwreRepositoryName :: Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesError (Maybe Text)
baartwreRepositoryName = lens _baartwreRepositoryName (\s a -> s {_baartwreRepositoryName = a})

-- | An error message that provides details about why the repository name was not found or not valid.
baartwreErrorMessage :: Lens' BatchAssociateApprovalRuleTemplateWithRepositoriesError (Maybe Text)
baartwreErrorMessage = lens _baartwreErrorMessage (\s a -> s {_baartwreErrorMessage = a})

instance
  FromJSON
    BatchAssociateApprovalRuleTemplateWithRepositoriesError
  where
  parseJSON =
    withObject
      "BatchAssociateApprovalRuleTemplateWithRepositoriesError"
      ( \x ->
          BatchAssociateApprovalRuleTemplateWithRepositoriesError'
            <$> (x .:? "errorCode")
            <*> (x .:? "repositoryName")
            <*> (x .:? "errorMessage")
      )

instance
  Hashable
    BatchAssociateApprovalRuleTemplateWithRepositoriesError

instance
  NFData
    BatchAssociateApprovalRuleTemplateWithRepositoriesError
