{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.OrganizationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.OrganizationSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The representation of an organization.
--
--
--
-- /See:/ 'organizationSummary' smart constructor.
data OrganizationSummary = OrganizationSummary'
  { _osState ::
      !(Maybe Text),
    _osAlias :: !(Maybe Text),
    _osDefaultMailDomain :: !(Maybe Text),
    _osErrorMessage :: !(Maybe Text),
    _osOrganizationId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OrganizationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osState' - The state associated with the organization.
--
-- * 'osAlias' - The alias associated with the organization.
--
-- * 'osDefaultMailDomain' - The default email domain associated with the organization.
--
-- * 'osErrorMessage' - The error message associated with the organization. It is only present if unexpected behavior has occurred with regards to the organization. It provides insight or solutions regarding unexpected behavior.
--
-- * 'osOrganizationId' - The identifier associated with the organization.
organizationSummary ::
  OrganizationSummary
organizationSummary =
  OrganizationSummary'
    { _osState = Nothing,
      _osAlias = Nothing,
      _osDefaultMailDomain = Nothing,
      _osErrorMessage = Nothing,
      _osOrganizationId = Nothing
    }

-- | The state associated with the organization.
osState :: Lens' OrganizationSummary (Maybe Text)
osState = lens _osState (\s a -> s {_osState = a})

-- | The alias associated with the organization.
osAlias :: Lens' OrganizationSummary (Maybe Text)
osAlias = lens _osAlias (\s a -> s {_osAlias = a})

-- | The default email domain associated with the organization.
osDefaultMailDomain :: Lens' OrganizationSummary (Maybe Text)
osDefaultMailDomain = lens _osDefaultMailDomain (\s a -> s {_osDefaultMailDomain = a})

-- | The error message associated with the organization. It is only present if unexpected behavior has occurred with regards to the organization. It provides insight or solutions regarding unexpected behavior.
osErrorMessage :: Lens' OrganizationSummary (Maybe Text)
osErrorMessage = lens _osErrorMessage (\s a -> s {_osErrorMessage = a})

-- | The identifier associated with the organization.
osOrganizationId :: Lens' OrganizationSummary (Maybe Text)
osOrganizationId = lens _osOrganizationId (\s a -> s {_osOrganizationId = a})

instance FromJSON OrganizationSummary where
  parseJSON =
    withObject
      "OrganizationSummary"
      ( \x ->
          OrganizationSummary'
            <$> (x .:? "State")
            <*> (x .:? "Alias")
            <*> (x .:? "DefaultMailDomain")
            <*> (x .:? "ErrorMessage")
            <*> (x .:? "OrganizationId")
      )

instance Hashable OrganizationSummary

instance NFData OrganizationSummary
