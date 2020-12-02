{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.SecurityProfileSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.SecurityProfileSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a security profile.
--
--
--
-- /See:/ 'securityProfileSummary' smart constructor.
data SecurityProfileSummary = SecurityProfileSummary'
  { _spsARN ::
      !(Maybe Text),
    _spsName :: !(Maybe Text),
    _spsId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SecurityProfileSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spsARN' - The Amazon Resource Name (ARN) of the security profile.
--
-- * 'spsName' - The name of the security profile.
--
-- * 'spsId' - The identifier of the security profile.
securityProfileSummary ::
  SecurityProfileSummary
securityProfileSummary =
  SecurityProfileSummary'
    { _spsARN = Nothing,
      _spsName = Nothing,
      _spsId = Nothing
    }

-- | The Amazon Resource Name (ARN) of the security profile.
spsARN :: Lens' SecurityProfileSummary (Maybe Text)
spsARN = lens _spsARN (\s a -> s {_spsARN = a})

-- | The name of the security profile.
spsName :: Lens' SecurityProfileSummary (Maybe Text)
spsName = lens _spsName (\s a -> s {_spsName = a})

-- | The identifier of the security profile.
spsId :: Lens' SecurityProfileSummary (Maybe Text)
spsId = lens _spsId (\s a -> s {_spsId = a})

instance FromJSON SecurityProfileSummary where
  parseJSON =
    withObject
      "SecurityProfileSummary"
      ( \x ->
          SecurityProfileSummary'
            <$> (x .:? "Arn") <*> (x .:? "Name") <*> (x .:? "Id")
      )

instance Hashable SecurityProfileSummary

instance NFData SecurityProfileSummary
