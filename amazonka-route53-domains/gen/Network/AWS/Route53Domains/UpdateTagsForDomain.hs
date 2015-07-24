{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.UpdateTagsForDomain
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation adds or updates tags for a specified domain.
--
-- All tag operations are eventually consistent; subsequent operations may
-- not immediately represent all issued operations.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-UpdateTagsForDomain.html>
module Network.AWS.Route53Domains.UpdateTagsForDomain
    (
    -- * Request
      UpdateTagsForDomain
    -- ** Request constructor
    , updateTagsForDomain
    -- ** Request lenses
    , utfdTagsToUpdate
    , utfdDomainName

    -- * Response
    , UpdateTagsForDomainResponse
    -- ** Response constructor
    , updateTagsForDomainResponse
    -- ** Response lenses
    , utfdrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types

-- | The UpdateTagsForDomainRequest includes the following elements.
--
-- /See:/ 'updateTagsForDomain' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'utfdTagsToUpdate'
--
-- * 'utfdDomainName'
data UpdateTagsForDomain = UpdateTagsForDomain'
    { _utfdTagsToUpdate :: !(Maybe [Tag])
    , _utfdDomainName   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateTagsForDomain' smart constructor.
updateTagsForDomain :: Text -> UpdateTagsForDomain
updateTagsForDomain pDomainName_ =
    UpdateTagsForDomain'
    { _utfdTagsToUpdate = Nothing
    , _utfdDomainName = pDomainName_
    }

-- | A list of the tag keys and values that you want to add or update. If you
-- specify a key that already exists, the corresponding value will be
-- replaced.
--
-- Type: A complex type containing a list of tags
--
-- Default: None
--
-- Required: No
--
-- \'>
--
-- Each tag includes the following elements:
--
-- -   Key
--
--     The key (name) of a tag.
--
--     Type: String
--
--     Default: None
--
--     Valid values: Unicode characters including alphanumeric, space, and
--     \".:\/=+\\-\@\"
--
--     Constraints: Each key can be 1-128 characters long.
--
--     Required: Yes
--
-- -   Value
--
--     The value of a tag.
--
--     Type: String
--
--     Default: None
--
--     Valid values: Unicode characters including alphanumeric, space, and
--     \".:\/=+\\-\@\"
--
--     Constraints: Each value can be 0-256 characters long.
--
--     Required: Yes
--
utfdTagsToUpdate :: Lens' UpdateTagsForDomain [Tag]
utfdTagsToUpdate = lens _utfdTagsToUpdate (\ s a -> s{_utfdTagsToUpdate = a}) . _Default;

-- | The domain for which you want to add or update tags.
--
-- The name of a domain.
--
-- Type: String
--
-- Default: None
--
-- Constraints: The domain name can contain only the letters a through z,
-- the numbers 0 through 9, and hyphen (-). Hyphens are allowed only when
-- theyaposre surrounded by letters, numbers, or other hyphens. You
-- canapost specify a hyphen at the beginning or end of a label. To specify
-- an Internationalized Domain Name, you must convert the name to Punycode.
--
-- Required: Yes
utfdDomainName :: Lens' UpdateTagsForDomain Text
utfdDomainName = lens _utfdDomainName (\ s a -> s{_utfdDomainName = a});

instance AWSRequest UpdateTagsForDomain where
        type Sv UpdateTagsForDomain = Route53Domains
        type Rs UpdateTagsForDomain =
             UpdateTagsForDomainResponse
        request = postJSON "UpdateTagsForDomain"
        response
          = receiveJSON
              (\ s h x ->
                 UpdateTagsForDomainResponse' <$> (pure (fromEnum s)))

instance ToHeaders UpdateTagsForDomain where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.UpdateTagsForDomain" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateTagsForDomain where
        toJSON UpdateTagsForDomain'{..}
          = object
              ["TagsToUpdate" .= _utfdTagsToUpdate,
               "DomainName" .= _utfdDomainName]

instance ToPath UpdateTagsForDomain where
        toPath = const "/"

instance ToQuery UpdateTagsForDomain where
        toQuery = const mempty

-- | /See:/ 'updateTagsForDomainResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'utfdrsStatus'
newtype UpdateTagsForDomainResponse = UpdateTagsForDomainResponse'
    { _utfdrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateTagsForDomainResponse' smart constructor.
updateTagsForDomainResponse :: Int -> UpdateTagsForDomainResponse
updateTagsForDomainResponse pStatus_ =
    UpdateTagsForDomainResponse'
    { _utfdrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
utfdrsStatus :: Lens' UpdateTagsForDomainResponse Int
utfdrsStatus = lens _utfdrsStatus (\ s a -> s{_utfdrsStatus = a});
