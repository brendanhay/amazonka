{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.EnabledServicePrincipal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.EnabledServicePrincipal where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A structure that contains details of a service principal that represents an AWS service that is enabled to integrate with AWS Organizations.
--
--
--
-- /See:/ 'enabledServicePrincipal' smart constructor.
data EnabledServicePrincipal = EnabledServicePrincipal'
  { _espServicePrincipal ::
      !(Maybe Text),
    _espDateEnabled :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnabledServicePrincipal' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'espServicePrincipal' - The name of the service principal. This is typically in the form of a URL, such as: @/servicename/ .amazonaws.com@ .
--
-- * 'espDateEnabled' - The date that the service principal was enabled for integration with AWS Organizations.
enabledServicePrincipal ::
  EnabledServicePrincipal
enabledServicePrincipal =
  EnabledServicePrincipal'
    { _espServicePrincipal = Nothing,
      _espDateEnabled = Nothing
    }

-- | The name of the service principal. This is typically in the form of a URL, such as: @/servicename/ .amazonaws.com@ .
espServicePrincipal :: Lens' EnabledServicePrincipal (Maybe Text)
espServicePrincipal = lens _espServicePrincipal (\s a -> s {_espServicePrincipal = a})

-- | The date that the service principal was enabled for integration with AWS Organizations.
espDateEnabled :: Lens' EnabledServicePrincipal (Maybe UTCTime)
espDateEnabled = lens _espDateEnabled (\s a -> s {_espDateEnabled = a}) . mapping _Time

instance FromJSON EnabledServicePrincipal where
  parseJSON =
    withObject
      "EnabledServicePrincipal"
      ( \x ->
          EnabledServicePrincipal'
            <$> (x .:? "ServicePrincipal") <*> (x .:? "DateEnabled")
      )

instance Hashable EnabledServicePrincipal

instance NFData EnabledServicePrincipal
