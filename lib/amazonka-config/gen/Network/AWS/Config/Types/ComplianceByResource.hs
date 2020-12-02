{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ComplianceByResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ComplianceByResource where

import Network.AWS.Config.Types.Compliance
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Indicates whether an AWS resource that is evaluated according to one or more AWS Config rules is compliant. A resource is compliant if it complies with all of the rules that evaluate it. A resource is noncompliant if it does not comply with one or more of these rules.
--
--
--
-- /See:/ 'complianceByResource' smart constructor.
data ComplianceByResource = ComplianceByResource'
  { _cbrResourceId ::
      !(Maybe Text),
    _cbrResourceType :: !(Maybe Text),
    _cbrCompliance :: !(Maybe Compliance)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ComplianceByResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbrResourceId' - The ID of the AWS resource that was evaluated.
--
-- * 'cbrResourceType' - The type of the AWS resource that was evaluated.
--
-- * 'cbrCompliance' - Indicates whether the AWS resource complies with all of the AWS Config rules that evaluated it.
complianceByResource ::
  ComplianceByResource
complianceByResource =
  ComplianceByResource'
    { _cbrResourceId = Nothing,
      _cbrResourceType = Nothing,
      _cbrCompliance = Nothing
    }

-- | The ID of the AWS resource that was evaluated.
cbrResourceId :: Lens' ComplianceByResource (Maybe Text)
cbrResourceId = lens _cbrResourceId (\s a -> s {_cbrResourceId = a})

-- | The type of the AWS resource that was evaluated.
cbrResourceType :: Lens' ComplianceByResource (Maybe Text)
cbrResourceType = lens _cbrResourceType (\s a -> s {_cbrResourceType = a})

-- | Indicates whether the AWS resource complies with all of the AWS Config rules that evaluated it.
cbrCompliance :: Lens' ComplianceByResource (Maybe Compliance)
cbrCompliance = lens _cbrCompliance (\s a -> s {_cbrCompliance = a})

instance FromJSON ComplianceByResource where
  parseJSON =
    withObject
      "ComplianceByResource"
      ( \x ->
          ComplianceByResource'
            <$> (x .:? "ResourceId")
            <*> (x .:? "ResourceType")
            <*> (x .:? "Compliance")
      )

instance Hashable ComplianceByResource

instance NFData ComplianceByResource
