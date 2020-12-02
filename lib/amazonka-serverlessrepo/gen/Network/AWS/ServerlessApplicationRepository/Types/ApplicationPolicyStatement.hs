{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.ApplicationPolicyStatement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.ApplicationPolicyStatement where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Policy statement applied to the application.
--
--
--
-- /See:/ 'applicationPolicyStatement' smart constructor.
data ApplicationPolicyStatement = ApplicationPolicyStatement'
  { _apsStatementId ::
      !(Maybe Text),
    _apsPrincipalOrgIds ::
      !(Maybe [Text]),
    _apsPrincipals :: ![Text],
    _apsActions :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApplicationPolicyStatement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apsStatementId' - A unique ID for the statement.
--
-- * 'apsPrincipalOrgIds' - An array of PrinciplalOrgIDs, which corresponds to AWS IAM <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_condition-keys.html#principal-org-id aws:PrincipalOrgID> global condition key.
--
-- * 'apsPrincipals' - An array of AWS account IDs, or * to make the application public.
--
-- * 'apsActions' - For the list of actions supported for this operation, see <https://docs.aws.amazon.com/serverlessrepo/latest/devguide/access-control-resource-based.html#application-permissions Application   Permissions> .
applicationPolicyStatement ::
  ApplicationPolicyStatement
applicationPolicyStatement =
  ApplicationPolicyStatement'
    { _apsStatementId = Nothing,
      _apsPrincipalOrgIds = Nothing,
      _apsPrincipals = mempty,
      _apsActions = mempty
    }

-- | A unique ID for the statement.
apsStatementId :: Lens' ApplicationPolicyStatement (Maybe Text)
apsStatementId = lens _apsStatementId (\s a -> s {_apsStatementId = a})

-- | An array of PrinciplalOrgIDs, which corresponds to AWS IAM <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_condition-keys.html#principal-org-id aws:PrincipalOrgID> global condition key.
apsPrincipalOrgIds :: Lens' ApplicationPolicyStatement [Text]
apsPrincipalOrgIds = lens _apsPrincipalOrgIds (\s a -> s {_apsPrincipalOrgIds = a}) . _Default . _Coerce

-- | An array of AWS account IDs, or * to make the application public.
apsPrincipals :: Lens' ApplicationPolicyStatement [Text]
apsPrincipals = lens _apsPrincipals (\s a -> s {_apsPrincipals = a}) . _Coerce

-- | For the list of actions supported for this operation, see <https://docs.aws.amazon.com/serverlessrepo/latest/devguide/access-control-resource-based.html#application-permissions Application   Permissions> .
apsActions :: Lens' ApplicationPolicyStatement [Text]
apsActions = lens _apsActions (\s a -> s {_apsActions = a}) . _Coerce

instance FromJSON ApplicationPolicyStatement where
  parseJSON =
    withObject
      "ApplicationPolicyStatement"
      ( \x ->
          ApplicationPolicyStatement'
            <$> (x .:? "statementId")
            <*> (x .:? "principalOrgIDs" .!= mempty)
            <*> (x .:? "principals" .!= mempty)
            <*> (x .:? "actions" .!= mempty)
      )

instance Hashable ApplicationPolicyStatement

instance NFData ApplicationPolicyStatement

instance ToJSON ApplicationPolicyStatement where
  toJSON ApplicationPolicyStatement' {..} =
    object
      ( catMaybes
          [ ("statementId" .=) <$> _apsStatementId,
            ("principalOrgIDs" .=) <$> _apsPrincipalOrgIds,
            Just ("principals" .= _apsPrincipals),
            Just ("actions" .= _apsActions)
          ]
      )
