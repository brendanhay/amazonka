{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdateRepositoryName
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Renames a repository.
--
-- <http://docs.aws.amazon.com/codecommit/latest/APIReference/API_UpdateRepositoryName.html>
module Network.AWS.CodeCommit.UpdateRepositoryName
    (
    -- * Request
      UpdateRepositoryName
    -- ** Request constructor
    , updateRepositoryName
    -- ** Request lenses
    , urnrqOldName
    , urnrqNewName

    -- * Response
    , UpdateRepositoryNameResponse
    -- ** Response constructor
    , updateRepositoryNameResponse
    ) where

import           Network.AWS.CodeCommit.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of an update repository description operation.
--
-- /See:/ 'updateRepositoryName' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'urnrqOldName'
--
-- * 'urnrqNewName'
data UpdateRepositoryName = UpdateRepositoryName'
    { _urnrqOldName :: !Text
    , _urnrqNewName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateRepositoryName' smart constructor.
updateRepositoryName :: Text -> Text -> UpdateRepositoryName
updateRepositoryName pOldName pNewName =
    UpdateRepositoryName'
    { _urnrqOldName = pOldName
    , _urnrqNewName = pNewName
    }

-- | FIXME: Undocumented member.
urnrqOldName :: Lens' UpdateRepositoryName Text
urnrqOldName = lens _urnrqOldName (\ s a -> s{_urnrqOldName = a});

-- | FIXME: Undocumented member.
urnrqNewName :: Lens' UpdateRepositoryName Text
urnrqNewName = lens _urnrqNewName (\ s a -> s{_urnrqNewName = a});

instance AWSRequest UpdateRepositoryName where
        type Sv UpdateRepositoryName = CodeCommit
        type Rs UpdateRepositoryName =
             UpdateRepositoryNameResponse
        request = postJSON
        response = receiveNull UpdateRepositoryNameResponse'

instance ToHeaders UpdateRepositoryName where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.UpdateRepositoryName" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateRepositoryName where
        toJSON UpdateRepositoryName'{..}
          = object
              ["oldName" .= _urnrqOldName,
               "newName" .= _urnrqNewName]

instance ToPath UpdateRepositoryName where
        toPath = const "/"

instance ToQuery UpdateRepositoryName where
        toQuery = const mempty

-- | /See:/ 'updateRepositoryNameResponse' smart constructor.
data UpdateRepositoryNameResponse =
    UpdateRepositoryNameResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateRepositoryNameResponse' smart constructor.
updateRepositoryNameResponse :: UpdateRepositoryNameResponse
updateRepositoryNameResponse = UpdateRepositoryNameResponse'
