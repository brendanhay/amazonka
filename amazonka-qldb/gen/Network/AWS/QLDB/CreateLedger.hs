{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.QLDB.CreateLedger
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.QLDB.CreateLedger
    (
    -- * Creating a Request
      createLedger
    , CreateLedger
    -- * Request Lenses
    , clDeletionProtection
    , clTags
    , clName
    , clPermissionsMode

    -- * Destructuring the Response
    , createLedgerResponse
    , CreateLedgerResponse
    -- * Response Lenses
    , clrsState
    , clrsDeletionProtection
    , clrsARN
    , clrsName
    , clrsCreationDateTime
    , clrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.QLDB.Types
import Network.AWS.QLDB.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createLedger' smart constructor.
data CreateLedger = CreateLedger'
  { _clDeletionProtection :: !(Maybe Bool)
  , _clTags               :: !(Maybe (Map Text Text))
  , _clName               :: !Text
  , _clPermissionsMode    :: !PermissionsMode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLedger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clDeletionProtection' - Undocumented member.
--
-- * 'clTags' - Undocumented member.
--
-- * 'clName' - Undocumented member.
--
-- * 'clPermissionsMode' - Undocumented member.
createLedger
    :: Text -- ^ 'clName'
    -> PermissionsMode -- ^ 'clPermissionsMode'
    -> CreateLedger
createLedger pName_ pPermissionsMode_ =
  CreateLedger'
    { _clDeletionProtection = Nothing
    , _clTags = Nothing
    , _clName = pName_
    , _clPermissionsMode = pPermissionsMode_
    }


-- | Undocumented member.
clDeletionProtection :: Lens' CreateLedger (Maybe Bool)
clDeletionProtection = lens _clDeletionProtection (\ s a -> s{_clDeletionProtection = a})

-- | Undocumented member.
clTags :: Lens' CreateLedger (HashMap Text Text)
clTags = lens _clTags (\ s a -> s{_clTags = a}) . _Default . _Map

-- | Undocumented member.
clName :: Lens' CreateLedger Text
clName = lens _clName (\ s a -> s{_clName = a})

-- | Undocumented member.
clPermissionsMode :: Lens' CreateLedger PermissionsMode
clPermissionsMode = lens _clPermissionsMode (\ s a -> s{_clPermissionsMode = a})

instance AWSRequest CreateLedger where
        type Rs CreateLedger = CreateLedgerResponse
        request = postJSON qldb
        response
          = receiveJSON
              (\ s h x ->
                 CreateLedgerResponse' <$>
                   (x .?> "State") <*> (x .?> "DeletionProtection") <*>
                     (x .?> "Arn")
                     <*> (x .?> "Name")
                     <*> (x .?> "CreationDateTime")
                     <*> (pure (fromEnum s)))

instance Hashable CreateLedger where

instance NFData CreateLedger where

instance ToHeaders CreateLedger where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON CreateLedger where
        toJSON CreateLedger'{..}
          = object
              (catMaybes
                 [("DeletionProtection" .=) <$> _clDeletionProtection,
                  ("Tags" .=) <$> _clTags, Just ("Name" .= _clName),
                  Just ("PermissionsMode" .= _clPermissionsMode)])

instance ToPath CreateLedger where
        toPath = const "/ledgers"

instance ToQuery CreateLedger where
        toQuery = const mempty

-- | /See:/ 'createLedgerResponse' smart constructor.
data CreateLedgerResponse = CreateLedgerResponse'
  { _clrsState              :: !(Maybe LedgerState)
  , _clrsDeletionProtection :: !(Maybe Bool)
  , _clrsARN                :: !(Maybe Text)
  , _clrsName               :: !(Maybe Text)
  , _clrsCreationDateTime   :: !(Maybe POSIX)
  , _clrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLedgerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clrsState' - Undocumented member.
--
-- * 'clrsDeletionProtection' - Undocumented member.
--
-- * 'clrsARN' - Undocumented member.
--
-- * 'clrsName' - Undocumented member.
--
-- * 'clrsCreationDateTime' - Undocumented member.
--
-- * 'clrsResponseStatus' - -- | The response status code.
createLedgerResponse
    :: Int -- ^ 'clrsResponseStatus'
    -> CreateLedgerResponse
createLedgerResponse pResponseStatus_ =
  CreateLedgerResponse'
    { _clrsState = Nothing
    , _clrsDeletionProtection = Nothing
    , _clrsARN = Nothing
    , _clrsName = Nothing
    , _clrsCreationDateTime = Nothing
    , _clrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
clrsState :: Lens' CreateLedgerResponse (Maybe LedgerState)
clrsState = lens _clrsState (\ s a -> s{_clrsState = a})

-- | Undocumented member.
clrsDeletionProtection :: Lens' CreateLedgerResponse (Maybe Bool)
clrsDeletionProtection = lens _clrsDeletionProtection (\ s a -> s{_clrsDeletionProtection = a})

-- | Undocumented member.
clrsARN :: Lens' CreateLedgerResponse (Maybe Text)
clrsARN = lens _clrsARN (\ s a -> s{_clrsARN = a})

-- | Undocumented member.
clrsName :: Lens' CreateLedgerResponse (Maybe Text)
clrsName = lens _clrsName (\ s a -> s{_clrsName = a})

-- | Undocumented member.
clrsCreationDateTime :: Lens' CreateLedgerResponse (Maybe UTCTime)
clrsCreationDateTime = lens _clrsCreationDateTime (\ s a -> s{_clrsCreationDateTime = a}) . mapping _Time

-- | -- | The response status code.
clrsResponseStatus :: Lens' CreateLedgerResponse Int
clrsResponseStatus = lens _clrsResponseStatus (\ s a -> s{_clrsResponseStatus = a})

instance NFData CreateLedgerResponse where
