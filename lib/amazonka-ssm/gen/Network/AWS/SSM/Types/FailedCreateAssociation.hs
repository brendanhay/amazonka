{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.FailedCreateAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.FailedCreateAssociation where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.CreateAssociationBatchRequestEntry
import Network.AWS.SSM.Types.Fault

-- | Describes a failed association.
--
--
--
-- /See:/ 'failedCreateAssociation' smart constructor.
data FailedCreateAssociation = FailedCreateAssociation'
  { _fcaEntry ::
      !(Maybe CreateAssociationBatchRequestEntry),
    _fcaFault :: !(Maybe Fault),
    _fcaMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FailedCreateAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcaEntry' - The association.
--
-- * 'fcaFault' - The source of the failure.
--
-- * 'fcaMessage' - A description of the failure.
failedCreateAssociation ::
  FailedCreateAssociation
failedCreateAssociation =
  FailedCreateAssociation'
    { _fcaEntry = Nothing,
      _fcaFault = Nothing,
      _fcaMessage = Nothing
    }

-- | The association.
fcaEntry :: Lens' FailedCreateAssociation (Maybe CreateAssociationBatchRequestEntry)
fcaEntry = lens _fcaEntry (\s a -> s {_fcaEntry = a})

-- | The source of the failure.
fcaFault :: Lens' FailedCreateAssociation (Maybe Fault)
fcaFault = lens _fcaFault (\s a -> s {_fcaFault = a})

-- | A description of the failure.
fcaMessage :: Lens' FailedCreateAssociation (Maybe Text)
fcaMessage = lens _fcaMessage (\s a -> s {_fcaMessage = a})

instance FromJSON FailedCreateAssociation where
  parseJSON =
    withObject
      "FailedCreateAssociation"
      ( \x ->
          FailedCreateAssociation'
            <$> (x .:? "Entry") <*> (x .:? "Fault") <*> (x .:? "Message")
      )

instance Hashable FailedCreateAssociation

instance NFData FailedCreateAssociation
