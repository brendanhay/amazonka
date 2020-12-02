{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.ContactFlowSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.ContactFlowSummary where

import Network.AWS.Connect.Types.ContactFlowType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains summary information about a contact flow.
--
--
-- You can also create and update contact flows using the <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language> .
--
--
-- /See:/ 'contactFlowSummary' smart constructor.
data ContactFlowSummary = ContactFlowSummary'
  { _cfsARN ::
      !(Maybe Text),
    _cfsName :: !(Maybe Text),
    _cfsContactFlowType :: !(Maybe ContactFlowType),
    _cfsId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContactFlowSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfsARN' - The Amazon Resource Name (ARN) of the contact flow.
--
-- * 'cfsName' - The name of the contact flow.
--
-- * 'cfsContactFlowType' - The type of contact flow.
--
-- * 'cfsId' - The identifier of the contact flow.
contactFlowSummary ::
  ContactFlowSummary
contactFlowSummary =
  ContactFlowSummary'
    { _cfsARN = Nothing,
      _cfsName = Nothing,
      _cfsContactFlowType = Nothing,
      _cfsId = Nothing
    }

-- | The Amazon Resource Name (ARN) of the contact flow.
cfsARN :: Lens' ContactFlowSummary (Maybe Text)
cfsARN = lens _cfsARN (\s a -> s {_cfsARN = a})

-- | The name of the contact flow.
cfsName :: Lens' ContactFlowSummary (Maybe Text)
cfsName = lens _cfsName (\s a -> s {_cfsName = a})

-- | The type of contact flow.
cfsContactFlowType :: Lens' ContactFlowSummary (Maybe ContactFlowType)
cfsContactFlowType = lens _cfsContactFlowType (\s a -> s {_cfsContactFlowType = a})

-- | The identifier of the contact flow.
cfsId :: Lens' ContactFlowSummary (Maybe Text)
cfsId = lens _cfsId (\s a -> s {_cfsId = a})

instance FromJSON ContactFlowSummary where
  parseJSON =
    withObject
      "ContactFlowSummary"
      ( \x ->
          ContactFlowSummary'
            <$> (x .:? "Arn")
            <*> (x .:? "Name")
            <*> (x .:? "ContactFlowType")
            <*> (x .:? "Id")
      )

instance Hashable ContactFlowSummary

instance NFData ContactFlowSummary
